(* Branch-aware git↔edit walking algorithm.

   Folds over a time-sorted stream of EditGroup and Commit events per
   branch, maintaining each file's current content as a direct string
   (no unified-diff round-trip). Writes go via handlers: Irmin for blob
   reads, SQLite [edit_links] for saves.

   Why no Patch library: we tried storing a chain of [Patch.t] and
   materialising via [Patch.patch] on the commit-base blob. That lost
   byte fidelity — Writes that produced byte-exact committed content
   still looked like "human edits" at reconcile time because the
   round-trip didn't preserve exact bytes. Tracking the resulting
   string directly avoids the issue entirely. *)

open Lwt.Syntax
open Git_link_types

module SMap = Map.Make(String)

(* ---------- Domain types ---------- *)

type file_state =
  | Content of string   (* walker's current belief about the file's bytes *)
  | Deleted
  | Absent

type edit_status =
  | Pending
  | Committed of string
  | Dead

type edit_origin = Claude | Human

type stack_entry = {
  origin : edit_origin;
  edits : (edit * edit_status) list;
  timestamp : float;
}

type branch_state = {
  file_states : file_state SMap.t;
  stack : stack_entry list;
  last_commit : string;
  branch : string;
}

type commit_file_change =
  | FileCreated of string
  | FileDeleted of string
  | FileEdited of string * string

type commit_info = {
  sha : string;
  timestamp : float;
  changes : (string * commit_file_change) list;
}

type event =
  | EditGroup of edit list
  | CommitEvent of commit_info

(* ---------- Handler types ---------- *)

type handlers = {
  (* Read the file's blob at [commit] from git (Irmin). Returns None
     if the file didn't exist at that commit. *)
  read_blob : commit:string -> file:string -> string option Lwt.t;
  save : stack_entry -> unit Lwt.t;
}

(* ---------- Helpers ---------- *)

let make_human_edit ~file ~old_string ~new_string ~timestamp ~branch =
  let edit_key = Printf.sprintf "human:%s:%s" file
      (Digest.string (old_string ^ "\x00" ^ new_string) |> Digest.to_hex) in
  { edit_key; file_base = Filename.basename file; file_path = file;
    old_string; new_string;
    replace_all = false; timestamp;
    session_id = "human"; interaction_index = 0;
    turn_idx = 0; entry_idx = 0;
    git_branch = branch }

let complete e =
  List.for_all (fun (_, s) -> s <> Pending) e.edits

let flush ~h stack =
  let done_, rest = List.partition complete stack in
  let* () = Lwt_list.iter_s h.save done_ in
  Lwt.return rest

(* ---------- Stack operations ---------- *)

let mark_dead ~h stack file ts =
  List.map (fun (e : stack_entry) ->
    if e.timestamp <= ts then
      { e with edits = List.map (fun (ed, s) ->
          if ed.file_path = file && s <> Dead
          then (ed, Dead) else (ed, s)) e.edits }
    else e) stack
  |> flush ~h

let assign ~h stack file sha ts =
  List.map (fun (e : stack_entry) ->
    if e.timestamp <= ts then
      { e with edits = List.map (fun (ed, s) ->
          if ed.file_path = file && s = Pending
          then (ed, Committed sha) else (ed, s)) e.edits }
    else e) stack
  |> flush ~h

(* ---------- State access ---------- *)

let set_file st file c =
  { st with file_states = SMap.add file c st.file_states }

let get_state st file =
  match SMap.find_opt file st.file_states with
  | Some s -> s
  | None -> Absent

(* On first access, populate the cache from [last_commit]'s blob. *)
let ensure_file ~h st file =
  match SMap.find_opt file st.file_states with
  | Some s -> Lwt.return (s, st)
  | None ->
    let* content = h.read_blob ~commit:st.last_commit ~file in
    let s = match content with
      | Some c -> Content c
      | None -> Absent in
    Lwt.return (s, set_file st file s)

(* Walker's view of the file's bytes right now. *)
let materialise ~h st file =
  let* _, st = ensure_file ~h st file in
  match get_state st file with
  | Content s -> Lwt.return (Some s, st)
  | Absent | Deleted -> Lwt.return (None, st)

(* ---------- Reconcile ---------- *)

(* If walker-state != commit-actual, push a Human edit representing the
   drift and snap state to [actual] so the next walk continues from
   reality. [before] is the parent commit's content; preferred as the
   human edit's old_string so the rendered diff matches git show. *)
let reconcile ~h st file ~actual ?before ts =
  let* expected, st = materialise ~h st file in
  let actual_s = Option.value actual ~default:"" in
  let expected_s = Option.value expected ~default:"" in
  if expected_s = actual_s then Lwt.return st
  else begin
    let diff_old = Option.value before ~default:expected_s in
    let hu = make_human_edit ~file
        ~old_string:diff_old ~new_string:actual_s
        ~timestamp:ts ~branch:st.branch in
    let entry = { origin = Human; edits = [hu, Pending]; timestamp = ts } in
    let fs = match actual with
      | Some c -> Content c
      | None -> Deleted in
    Lwt.return { (set_file st file fs) with stack = st.stack @ [entry] }
  end

(* ---------- Edit group event ---------- *)

let apply_one ~h st (e : edit) =
  let file = e.file_path in
  let* state, st = ensure_file ~h st file in

  if e.old_string = "" then
    (* Write: overwrite walker content. DO NOT mark prior Pending
       Edits Dead — an Edit on an Absent file genuinely means the
       file existed on disk; those edits led up to this Write and
       should still be attributed to the commit that eventually
       absorbs it. *)
    let st = set_file st file (Content e.new_string) in
    Lwt.return (st, Pending)

  else match state with
  | Absent | Deleted ->
    (* File isn't in any ancestor commit the walker has processed
       yet, but Edit implies it exists on disk. Keep Pending — the
       next commit that touches this file will [assign] it. *)
    Lwt.return (st, Pending)

  | Content content ->
    match find_substring e.old_string content with
    | Some pos ->
      let olen = String.length e.old_string in
      let before = String.sub content 0 pos in
      let after = String.sub content (pos + olen)
          (String.length content - pos - olen) in
      let new_content = before ^ e.new_string ^ after in
      Lwt.return (set_file st file (Content new_content), Pending)

    | None ->
      (* Edit's old_string not in walker state → drift. Reconcile
         (push Human for the drift) and keep THIS edit Pending so
         the next commit can still attribute it. *)
      let* st = reconcile ~h st file
          ~actual:(Some e.old_string) e.timestamp in
      Lwt.return (st, Pending)

let process_edit_group ~h st (edits : edit list) =
  let ts = match edits with e :: _ -> e.timestamp | [] -> 0.0 in
  let* st, pairs = Lwt_list.fold_left_s (fun (st, acc) e ->
    let* st, status = apply_one ~h st e in
    Lwt.return (st, acc @ [e, status])
  ) (st, []) edits in
  let entry = { origin = Claude; edits = pairs; timestamp = ts } in
  if complete entry then
    let* () = h.save entry in
    Lwt.return st
  else
    Lwt.return { st with stack = st.stack @ [entry] }

(* ---------- Commit event ---------- *)

let process_commit ~h st (ci : commit_info) =
  let* st = Lwt_list.fold_left_s (fun st (file, change) ->
    let before, actual = match change with
      | FileCreated c -> None, Some c
      | FileEdited (b, c) -> Some b, Some c
      | FileDeleted b -> Some b, None in
    let* st = reconcile ~h st file ~actual ?before ci.timestamp in
    let* stack = assign ~h st.stack file ci.sha ci.timestamp in
    let st = { st with stack } in
    match actual with
    | Some c -> Lwt.return (set_file st file (Content c))
    | None -> Lwt.return (set_file st file Deleted)
  ) st ci.changes in
  Lwt.return { st with last_commit = ci.sha }

(* ---------- Main ---------- *)

let process_event ~h st = function
  | EditGroup edits -> process_edit_group ~h st edits
  | CommitEvent ci -> process_commit ~h st ci

(* End-of-walk: flush any Claude stack_entries still carrying Pending
   edits. Without this, Claude EditGroups whose timestamps fall after
   the last commit on their branch are silently dropped — every session
   done on a branch's current tip loses all its attributions. Human
   reconciliation entries are intentionally NOT flushed: they're pure
   drift-records, recomputed on the next walk. *)
let flush_pending_claude ~h stack =
  Lwt_list.iter_s (fun (e : stack_entry) ->
    match e.origin with
    | Claude -> h.save e
    | Human -> Lwt.return_unit) stack

let walk ~h ~branch ~last_commit events =
  let* final = Lwt_list.fold_left_s (process_event ~h)
    { file_states = SMap.empty; stack = []; last_commit; branch }
    events in
  let* () = flush_pending_claude ~h final.stack in
  Lwt.return final

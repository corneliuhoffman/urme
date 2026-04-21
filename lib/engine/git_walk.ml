(* Branch-aware git↔edit walking algorithm.

   Folds over a time-sorted stream of EditGroup and Commit events per
   branch, maintaining file states as patches on top of a base commit.

   Lwt-native: [walk] takes [materialise] and [save] as function
   parameters — V2 handlers in [Walk_handlers] bridge Irmin for reads
   and SQLite [edit_links] for writes. *)

open Lwt.Syntax
open Git_link_types

module SMap = Map.Make(String)

(* ---------- Domain types ---------- *)

type file_state =
  | Patches of Patch.t list
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
  materialise : commit:string -> file:string -> patches:Patch.t list
                -> string option Lwt.t;
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

let make_patch ~file ~old_content ~new_content =
  Patch.diff
    (Some (file, old_content))
    (Some (file, new_content))

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

(* Bootstrap file state from last_commit on first access.
   Returns (state, updated st) with the file cached. *)
let ensure_file ~h st file =
  match SMap.find_opt file st.file_states with
  | Some s -> Lwt.return (s, st)
  | None ->
    let* content = h.materialise ~commit:st.last_commit ~file ~patches:[] in
    let s = match content with
      | Some _ -> Patches []
      | None -> Absent in
    Lwt.return (s, set_file st file s)

let materialise ~h st file =
  match get_state st file with
  | Absent | Deleted -> Lwt.return_none
  | Patches ps -> h.materialise ~commit:st.last_commit ~file ~patches:ps

let current_patches st file =
  match get_state st file with
  | Patches ps -> ps
  | _ -> []

(* ---------- Reconcile ---------- *)

(* If expected != actual, push a human edit and append a delta patch
   so future materialisations produce [actual]. *)
let reconcile ~h st file ~actual ts =
  let* _, st = ensure_file ~h st file in
  let* expected = materialise ~h st file in
  let actual_s = Option.value actual ~default:"" in
  let expected_s = Option.value expected ~default:"" in
  if expected_s = actual_s then Lwt.return st
  else
    let hu = make_human_edit ~file
        ~old_string:expected_s ~new_string:actual_s
        ~timestamp:ts ~branch:st.branch in
    let entry = { origin = Human; edits = [hu, Pending]; timestamp = ts } in
    let delta = make_patch ~file ~old_content:expected_s ~new_content:actual_s in
    let new_patches = current_patches st file @ Option.to_list delta in
    let fs = match actual with
      | Some _ -> Patches new_patches
      | None -> Deleted in
    Lwt.return { (set_file st file fs) with stack = st.stack @ [entry] }

(* ---------- Edit group event ---------- *)

let apply_one ~h st (e : edit) =
  let file = e.file_path in
  let* state, st = ensure_file ~h st file in

  if e.old_string = "" then
    let* stack = mark_dead ~h st.stack file e.timestamp in
    let* expected = materialise ~h st file in
    let old_s = Option.value expected ~default:"" in
    let p = make_patch ~file ~old_content:old_s ~new_content:e.new_string in
    let new_patches = current_patches st file @ Option.to_list p in
    let st = set_file { st with stack } file (Patches new_patches) in
    Lwt.return (st, Pending)

  else match state with
  | Absent | Deleted -> Lwt.return (st, Dead)

  | Patches _ ->
    let* content = materialise ~h st file in
    match content with
    | None -> Lwt.return (st, Dead)
    | Some content ->
      match find_substring e.old_string content with
      | Some pos ->
        let olen = String.length e.old_string in
        let before = String.sub content 0 pos in
        let after = String.sub content (pos + olen)
            (String.length content - pos - olen) in
        let new_content = before ^ e.new_string ^ after in
        let p = make_patch ~file ~old_content:content ~new_content in
        let new_patches = current_patches st file @ Option.to_list p in
        Lwt.return (set_file st file (Patches new_patches), Pending)

      | None ->
        let* st = reconcile ~h st file ~actual:(Some e.old_string) e.timestamp in
        let p = make_patch ~file
            ~old_content:e.old_string ~new_content:e.new_string in
        let new_patches = current_patches st file @ Option.to_list p in
        Lwt.return (set_file st file (Patches new_patches), Pending)

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
    let actual = match change with
      | FileCreated c | FileEdited (_, c) -> Some c
      | FileDeleted _ -> None in
    let* st = reconcile ~h st file ~actual ci.timestamp in
    let* stack = assign ~h st.stack file ci.sha ci.timestamp in
    let st = { st with stack } in
    match actual with
    | Some _ -> Lwt.return (set_file st file (Patches []))
    | None -> Lwt.return (set_file st file Deleted)
  ) st ci.changes in
  Lwt.return { st with last_commit = ci.sha }

(* ---------- Main ---------- *)

let process_event ~h st = function
  | EditGroup edits -> process_edit_group ~h st edits
  | CommitEvent ci -> process_commit ~h st ci

let walk ~h ~branch ~last_commit events =
  Lwt_list.fold_left_s (process_event ~h)
    { file_states = SMap.empty; stack = []; last_commit; branch }
    events

(* Handlers for Git_walk: bridge the pure walking algorithm to
   real IO — Irmin for file reads, SQLite [edit_links] for writes. *)

open Lwt.Syntax
open Git_link_types

(* ---------- Materialise: read from Irmin + apply patches ---------- *)

let materialise ~repo ~commit ~file ~patches =
  let* base = Lwt.catch (fun () ->
    Urme_store.Project_store.read_blob ~repo ~sha:commit ~path:file
  ) (fun _ -> Lwt.return_none) in
  Lwt.return (List.fold_left (fun acc p ->
    Patch.patch ~cleanly:false acc p
  ) base patches)

(* ---------- Save: write edit linkage to SQLite ---------- *)

let row_of_edit ~origin ~branch (edit : edit) ~status =
  let commit_sha, diff_hash =
    match (status : Git_walk.edit_status) with
    | Committed sha ->
      let dh = diff_hash_of_string (edit.old_string ^ "\x00" ^ edit.new_string) in
      Some sha, dh
    | Dead -> None, ""
    | Pending -> None, ""
  in
  let session_id = match origin with
    | Git_walk.Claude -> Some edit.session_id
    | Git_walk.Human -> None
  in
  { Urme_store.Edit_links.
    edit_key = edit.edit_key;
    session_id;
    turn_idx = edit.turn_idx;
    entry_idx = edit.entry_idx;
    file_base = edit.file_base;
    commit_sha;
    diff_hash;
    origin = (match origin with Git_walk.Claude -> "claude" | Git_walk.Human -> "human");
    branch;
    timestamp = edit.timestamp; }

(* Save a completed stack_entry: upsert one row per edit.
   Pending edits (still unmatched after the walk) are skipped — the
   entry wouldn't be passed to save() if any were still Pending under
   the walker's complete() check. *)
let save ~db (entry : Git_walk.stack_entry) =
  List.iter (fun (edit, status) ->
    match (status : Git_walk.edit_status) with
    | Pending -> ()
    | _ ->
      let row = row_of_edit ~origin:entry.origin
          ~branch:edit.git_branch edit ~status in
      Urme_store.Edit_links.upsert db row
  ) entry.edits;
  Lwt.return_unit

(* ---------- Build handlers record ---------- *)

let make ~repo ~db : Git_walk.handlers =
  { materialise = (fun ~commit ~file ~patches ->
      materialise ~repo ~commit ~file ~patches);
    save = (fun entry -> save ~db entry); }

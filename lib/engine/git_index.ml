(* Branch-aware index orchestrator.

   Loads saved git state, diffs against current git, walks each changed
   branch using Git_walk + Walk_handlers (SQLite sink). Updates saved
   state after each run. Cleans up orphaned linkages when branches are
   rebased or deleted. *)

open Lwt.Syntax
open Git_link_types

module SMap = Map.Make(String)

(* ---------- Commit info builder ---------- *)

(* Build a Git_walk.commit_info from a commit SHA.
   Uses git CLI for the file list (Irmin's Tree.diff has a bug that
   misses files), then reads blob contents via Irmin. *)
let commit_info_of ~cwd ~repo ~sha ~timestamp : Git_walk.commit_info Lwt.t =
  let* parent_raw = Lwt.catch (fun () ->
    Urme_git.Ops.run_git ~cwd ["rev-parse"; sha ^ "^"]
  ) (fun _ -> Lwt.return "") in
  let parent = String.trim parent_raw in
  let args = if parent = ""
    then ["diff-tree"; "--no-commit-id"; "--name-status"; "-r"; sha]
    else ["diff"; "--name-status"; parent; sha] in
  let* out = Lwt.catch (fun () -> Urme_git.Ops.run_git ~cwd args)
    (fun _ -> Lwt.return "") in
  let entries = String.split_on_char '\n' out
    |> List.filter_map (fun line ->
      match String.split_on_char '\t' (String.trim line) with
      | [status; path] when status <> "" && path <> "" ->
        Some (status, path)
      | _ -> None) in
  let* changes = Lwt_list.filter_map_s (fun (status, f) ->
    let* after = Urme_store.Project_store.read_blob ~repo ~sha ~path:f in
    let* before =
      if parent = "" then Lwt.return_none
      else Urme_store.Project_store.read_blob ~repo ~sha:parent ~path:f in
    match status.[0], before, after with
    | 'A', _, Some a -> Lwt.return_some (f, Git_walk.FileCreated a)
    | 'D', Some b, _ -> Lwt.return_some (f, Git_walk.FileDeleted b)
    | 'M', Some b, Some a -> Lwt.return_some (f, Git_walk.FileEdited (b, a))
    | _, None, Some a -> Lwt.return_some (f, Git_walk.FileCreated a)
    | _, Some b, None -> Lwt.return_some (f, Git_walk.FileDeleted b)
    | _, Some b, Some a -> Lwt.return_some (f, Git_walk.FileEdited (b, a))
    | _ -> Lwt.return_none
  ) entries in
  Lwt.return Git_walk.{ sha; timestamp; changes }

(* ---------- Edit grouping ---------- *)

let group_edits (edits : edit list) =
  let by_turn : (string * int, edit list) Hashtbl.t = Hashtbl.create 64 in
  List.iter (fun (e : edit) ->
    let key = (e.session_id, e.turn_idx) in
    let existing = try Hashtbl.find by_turn key with Not_found -> [] in
    Hashtbl.replace by_turn key (existing @ [e])
  ) edits;
  Hashtbl.fold (fun _ es acc -> es :: acc) by_turn []

(* ---------- Event stream ---------- *)

let build_events ~cwd ~repo ~branch_edits ~commits =
  let edit_groups = group_edits branch_edits in
  let edit_events = List.map (fun group ->
    let ts = match group with e :: _ -> e.timestamp | [] -> 0.0 in
    (ts, Git_walk.EditGroup group)
  ) edit_groups in
  let* commit_events = Lwt_list.map_p (fun (sha, ts, _msg) ->
    let+ ci = commit_info_of ~cwd ~repo ~sha ~timestamp:ts in
    (ts, Git_walk.CommitEvent ci)
  ) commits in
  ignore cwd;
  let all = edit_events @ commit_events in
  let sorted = List.sort (fun (a, _) (b, _) -> Float.compare a b) all in
  Lwt.return (List.map snd sorted)

(* ---------- Per-branch walk ---------- *)

let walk_branch ~cwd ~repo ~db
    ~branch ~edits ~since_sha ~until_sha =
  let since = if since_sha = "" then "" else since_sha ^ ".." in
  let range = since ^ until_sha in
  let* out = Lwt.catch (fun () ->
    Urme_git.Ops.run_git ~cwd
      ["log"; "--format=%H%n%at%n%s%n---"; range]
  ) (fun _ -> Lwt.return "") in
  let commits =
    let lines = String.split_on_char '\n' out in
    let rec parse = function
      | sha :: ts :: msg :: "---" :: rest ->
        (sha, (try float_of_string ts with _ -> 0.0), msg) :: parse rest
      | _ -> []
    in parse lines |> List.rev in
  let branch_edits = List.filter (fun (e : edit) -> e.git_branch = branch) edits in
  let* events = build_events ~cwd ~repo ~branch_edits ~commits in
  let last_commit = since_sha in
  let h = Walk_handlers.make ~repo ~db in
  let* _final = Git_walk.walk ~h ~branch ~last_commit events in
  let new_last_processed = match List.rev commits with
    | (sha, _, _) :: _ -> sha
    | [] -> since_sha in
  Lwt.return new_last_processed

(* ---------- Orphan cleanup ---------- *)

let all_reachable_shas ~cwd =
  let* out = Lwt.catch (fun () ->
    Urme_git.Ops.run_git ~cwd ["log"; "--all"; "--format=%H"]
  ) (fun _ -> Lwt.return "") in
  let shas = String.split_on_char '\n' out
             |> List.map String.trim
             |> List.filter (fun s -> s <> "") in
  Lwt.return shas

(* Clear commit_sha on any edit_links row whose stored SHA is unreachable.
   Runs after rebase/branch-delete events. *)
let cleanup_orphans ~cwd ~db =
  let* reachable = all_reachable_shas ~cwd in
  let n = Urme_store.Edit_links.cleanup_orphans db ~reachable_shas:reachable in
  Lwt.return n

(* ---------- Main entry point ---------- *)

let update ~project_dir ~db ~edits =
  let* repo = Urme_store.Project_store.open_repo ~project_dir in
  let saved = Git_state.load ~project_dir in
  let* changes = Branch_diff.diff ~cwd:project_dir ~saved in
  let new_state = ref saved in
  let needs_cleanup = ref false in
  let* () = Lwt_list.iter_s (fun change ->
    match change with
    | Branch_diff.Unchanged _ -> Lwt.return_unit
    | Branch_diff.NewBranch { name; tip } ->
      let* last_processed = walk_branch ~cwd:project_dir ~repo ~db
          ~branch:name ~edits ~since_sha:"" ~until_sha:tip in
      new_state := Git_state.update_branch !new_state name
          { tip; last_processed };
      Lwt.return_unit
    | Branch_diff.FastForward { name; old_tip; new_tip } ->
      let* last_processed = walk_branch ~cwd:project_dir ~repo ~db
          ~branch:name ~edits
          ~since_sha:old_tip ~until_sha:new_tip in
      new_state := Git_state.update_branch !new_state name
          { tip = new_tip; last_processed };
      Lwt.return_unit
    | Branch_diff.Rebased { name; old_tip = _; new_tip } ->
      needs_cleanup := true;
      let* last_processed = walk_branch ~cwd:project_dir ~repo ~db
          ~branch:name ~edits ~since_sha:"" ~until_sha:new_tip in
      new_state := Git_state.update_branch !new_state name
          { tip = new_tip; last_processed };
      Lwt.return_unit
    | Branch_diff.Merged { name; _ } ->
      new_state := Git_state.remove_branch !new_state name;
      Lwt.return_unit
    | Branch_diff.Deleted { name; _ } ->
      needs_cleanup := true;
      new_state := Git_state.remove_branch !new_state name;
      Lwt.return_unit
  ) changes in
  let* () = if !needs_cleanup then
    let* _n = cleanup_orphans ~cwd:project_dir ~db in
    Lwt.return_unit
  else Lwt.return_unit in
  Git_state.save ~project_dir !new_state;
  Lwt.return_unit

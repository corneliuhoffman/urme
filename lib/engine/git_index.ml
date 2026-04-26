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
    Hashtbl.replace by_turn key (e :: existing)
  ) edits;
  (* Sort groups by (session_id, turn_idx) for deterministic order.
     Within each group, sort by (entry_idx, timestamp) ascending —
     [edits_of_sessions] hands us a descending-timestamp list, so
     without this within-turn resort the walker applies entry N
     before entry 0 and subsequent Edits' [old_string] lookups fail. *)
  let groups_with_keys =
    Hashtbl.fold (fun (sid, ti) es acc -> ((sid, ti), es) :: acc) by_turn [] in
  List.sort (fun ((s1, t1), _) ((s2, t2), _) ->
    let c = String.compare s1 s2 in
    if c <> 0 then c else Int.compare t1 t2) groups_with_keys
  |> List.map (fun (_, es) ->
       List.sort (fun (a : edit) b ->
         let c = Int.compare a.entry_idx b.entry_idx in
         if c <> 0 then c else Float.compare a.timestamp b.timestamp) es)

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

let walk_branch ~cwd ~repo ~db ~min_ts
    ~branch ~edits ~since_sha ~until_sha =
  let since = if since_sha = "" then "" else since_sha ^ ".." in
  let range = since ^ until_sha in
  (* [--since] cuts the git log to commits AT OR AFTER min_ts. Commits
     older than the earliest JSONL can never be attributed to Claude —
     walking them only produces noisy human reconciliation rows and
     burns minutes of CPU per branch. *)
  let since_args =
    if min_ts > 0.0
    then ["--since"; Printf.sprintf "%.0f" min_ts]
    else [] in
  let* out = Lwt.catch (fun () ->
    Urme_git.Ops.run_git ~cwd
      (["log"; "--format=%H%n%at%n%s%n---"] @ since_args @ [range])
  ) (fun _ -> Lwt.return "") in
  let commits =
    let lines = String.split_on_char '\n' out in
    let rec parse = function
      | sha :: ts :: msg :: "---" :: rest ->
        (sha, (try float_of_string ts with _ -> 0.0), msg) :: parse rest
      | _ -> []
    in parse lines |> List.rev in
  let branch_edits = List.filter (fun (e : edit) -> e.git_branch = branch) edits in
  Printf.printf "  walk %s: %d commits, %d edits%!\n"
    branch (List.length commits) (List.length branch_edits);
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

(* Branches that still have Claude edits with NULL commit_sha. After
   re-extracting JSONLs (e.g. with corrected cross-machine paths), the
   branch tip hasn't moved so Branch_diff returns Unchanged and the
   walker would skip these branches forever. We treat that case as
   "needs relinking" and re-walk from scratch. *)
let branches_with_unlinked_claude_edits db =
  Urme_store.Db.query_fold db
    "SELECT DISTINCT branch FROM edit_links \
     WHERE origin = 'claude' AND commit_sha IS NULL \
       AND branch IS NOT NULL AND branch != ''"
    [] ~init:[] ~f:(fun acc cols ->
      Urme_store.Db.data_to_string cols.(0) :: acc)

(* ---------- Main entry point ---------- *)

let update ~project_dir ~db ~edits =
  let* repo = Urme_store.Project_store.open_repo ~project_dir in
  let saved = Git_state.load ~project_dir in
  let relink_branches =
    let s = Hashtbl.create 8 in
    List.iter (fun b -> Hashtbl.replace s b ())
      (branches_with_unlinked_claude_edits db);
    s in
  let* changes = Branch_diff.diff ~cwd:project_dir ~saved in
  (* Earliest JSONL-derived edit timestamp — anchor for [--since] pruning
     in walk_branch. 0.0 means "no edits, walk everything" (first-run
     safety). *)
  let min_ts = List.fold_left (fun acc (e : edit) ->
    if acc = 0.0 || e.timestamp < acc then e.timestamp else acc) 0.0 edits in
  (if min_ts > 0.0 then
     Printf.printf "building per-edit git links (since %.0f, %d branches to check)...\n%!"
       min_ts (List.length changes));
  let new_state = ref saved in
  let needs_cleanup = ref false in
  let* () = Lwt_list.iter_s (fun change ->
    match change with
    | Branch_diff.Unchanged { name; tip } when Hashtbl.mem relink_branches name ->
      Printf.printf "  re-linking %s (unlinked claude edits)\n%!" name;
      let* last_processed = walk_branch ~cwd:project_dir ~repo ~db ~min_ts
          ~branch:name ~edits ~since_sha:"" ~until_sha:tip in
      new_state := Git_state.update_branch !new_state name
          { tip; last_processed };
      Lwt.return_unit
    | Branch_diff.Unchanged _ -> Lwt.return_unit
    | Branch_diff.NewBranch { name; tip } ->
      let* last_processed = walk_branch ~cwd:project_dir ~repo ~db ~min_ts
          ~branch:name ~edits ~since_sha:"" ~until_sha:tip in
      new_state := Git_state.update_branch !new_state name
          { tip; last_processed };
      Lwt.return_unit
    | Branch_diff.FastForward { name; old_tip; new_tip } ->
      let* last_processed = walk_branch ~cwd:project_dir ~repo ~db ~min_ts
          ~branch:name ~edits
          ~since_sha:old_tip ~until_sha:new_tip in
      new_state := Git_state.update_branch !new_state name
          { tip = new_tip; last_processed };
      Lwt.return_unit
    | Branch_diff.Rebased { name; old_tip = _; new_tip } ->
      needs_cleanup := true;
      let* last_processed = walk_branch ~cwd:project_dir ~repo ~db ~min_ts
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

(* One-shot link pass from scratch inputs. Extract edits via
   Domainslib, then call [update]. Both [urme init] and the TUI's
   `i` / hourly resync go through here so the two paths can't
   diverge. *)
let run_once ~project_dir ~db =
  let pool = Domainslib.Task.setup_pool ~num_domains:4 () in
  let edits = Edit_extract.edits_of_sessions ~pool ~project_dir in
  Domainslib.Task.teardown_pool pool;
  update ~project_dir ~db ~edits

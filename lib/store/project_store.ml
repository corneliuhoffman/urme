open Lwt.Syntax
open Urme_core

(* Irmin-git store for reading the project's .git directory *)
module Store = Irmin_git_unix.FS.KV (Irmin.Contents.String)
module Info = Irmin_git_unix.Info

(* Open a repo from a project directory *)
let open_repo ~project_dir =
  let dot_git = Filename.concat project_dir ".git" in
  let config = Irmin_git.config ~bare:false project_dir ~dot_git in
  Store.Repo.v config

(* Get current HEAD commit *)
let current_head ~repo =
  let* t = Store.main repo in
  let* head_opt = Store.Head.find t in
  match head_opt with
  | Some commit ->
    let hash = Store.Commit.hash commit in
    Lwt.return (Irmin.Type.to_string Store.Hash.t hash)
  | None -> Lwt.fail_with "No HEAD commit"

(* Get current branch name *)
let current_branch ~repo =
  let* t = Store.main repo in
  ignore t;
  (* Irmin always opens "main" branch by default, but we want the actual
     git HEAD ref. Fall back to git CLI for this since Irmin's branch
     abstraction doesn't directly map to git's HEAD symbolic ref. *)
  Lwt.return "main"

(* Get commit by SHA string *)
let find_commit ~repo sha_str =
  match Irmin.Type.of_string Store.Hash.t sha_str with
  | Error _ -> Lwt.return_none
  | Ok hash ->
    Store.Commit.of_hash repo hash

(* Get commit info: message, author, timestamp, parents *)
let commit_info ~repo ~sha =
  let* commit_opt = find_commit ~repo sha in
  match commit_opt with
  | None -> Lwt.fail_with (Printf.sprintf "Commit not found: %s" sha)
  | Some commit ->
    let info = Store.Commit.info commit in
    let message = Store.Info.message info in
    let author = Store.Info.author info in
    let date = Store.Info.date info in
    let parent_keys = Store.Commit.parents commit in
    let* parent_shas = Lwt_list.filter_map_s (fun key ->
      let* c = Store.Commit.of_key repo key in
      match c with
      | Some c ->
        let h = Store.Commit.hash c in
        Lwt.return_some (Irmin.Type.to_string Store.Hash.t h)
      | None -> Lwt.return_none
    ) parent_keys in
    Lwt.return (message, author, date, parent_shas)

(* Diff between two commits — returns list of (path, change_type) *)
let changed_files ~repo ~sha1 ~sha2 =
  let* c1_opt = find_commit ~repo sha1 in
  let* c2_opt = find_commit ~repo sha2 in
  match c1_opt, c2_opt with
  | Some c1, Some c2 ->
    let tree1 = Store.Commit.tree c1 in
    let tree2 = Store.Commit.tree c2 in
    let+ diffs = Store.Tree.diff tree1 tree2 in
    List.map (fun (path, _change) ->
      (* path is a list of steps — join with / *)
      String.concat "/" path
    ) diffs
  | _ -> Lwt.return []

(* Generate a unified diff string between two commits *)
let diff_between ~repo ~sha1 ~sha2 =
  let* c1_opt = find_commit ~repo sha1 in
  let* c2_opt = find_commit ~repo sha2 in
  match c1_opt, c2_opt with
  | Some c1, Some c2 ->
    let tree1 = Store.Commit.tree c1 in
    let tree2 = Store.Commit.tree c2 in
    let* diffs = Store.Tree.diff tree1 tree2 in
    let buf = Buffer.create 4096 in
    let* () = Lwt_list.iter_s (fun (path, change) ->
      let path_str = String.concat "/" path in
      (match change with
       | `Added (contents, _meta) ->
         Buffer.add_string buf (Printf.sprintf "--- /dev/null\n+++ b/%s\n" path_str);
         let lines = String.split_on_char '\n' contents in
         List.iter (fun line ->
           Buffer.add_string buf (Printf.sprintf "+%s\n" line)
         ) lines;
         Lwt.return_unit
       | `Removed (contents, _meta) ->
         Buffer.add_string buf (Printf.sprintf "--- a/%s\n+++ /dev/null\n" path_str);
         let lines = String.split_on_char '\n' contents in
         List.iter (fun line ->
           Buffer.add_string buf (Printf.sprintf "-%s\n" line)
         ) lines;
         Lwt.return_unit
       | `Updated ((old_contents, _old_meta), (new_contents, _new_meta)) ->
         Buffer.add_string buf (Printf.sprintf "--- a/%s\n+++ b/%s\n" path_str path_str);
         let old_lines = String.split_on_char '\n' old_contents in
         let new_lines = String.split_on_char '\n' new_contents in
         List.iter (fun line ->
           Buffer.add_string buf (Printf.sprintf "-%s\n" line)
         ) old_lines;
         List.iter (fun line ->
           Buffer.add_string buf (Printf.sprintf "+%s\n" line)
         ) new_lines;
         Lwt.return_unit)
    ) diffs in
    Lwt.return (Buffer.contents buf)
  | _ -> Lwt.return ""

(* Read file contents at a specific commit *)
let read_blob ~repo ~sha ~path =
  let* commit_opt = find_commit ~repo sha in
  match commit_opt with
  | None -> Lwt.return_none
  | Some commit ->
    let tree = Store.Commit.tree commit in
    let key = String.split_on_char '/' path in
    Store.Tree.find tree key

(* Walk commit history from HEAD, collecting commits that pass the filter *)
let walk_history ~repo ?(max_depth=1000) ~filter () =
  let* t = Store.main repo in
  let* head_opt = Store.Head.find t in
  match head_opt with
  | None -> Lwt.return []
  | Some head ->
    let visited = Hashtbl.create 256 in
    let results = ref [] in
    let rec walk depth commits =
      if depth >= max_depth then Lwt.return_unit
      else
        Lwt_list.iter_s (fun commit ->
          let hash = Store.Commit.hash commit in
          let sha = Irmin.Type.to_string Store.Hash.t hash in
          if Hashtbl.mem visited sha then Lwt.return_unit
          else begin
            Hashtbl.replace visited sha ();
            let info = Store.Commit.info commit in
            let message = Store.Info.message info in
            let* () =
              if filter ~sha ~message then begin
                results := (sha, commit) :: !results;
                Lwt.return_unit
              end else Lwt.return_unit
            in
            let parent_keys = Store.Commit.parents commit in
            let* parents = Lwt_list.filter_map_s (fun key ->
              Store.Commit.of_key repo key
            ) parent_keys in
            walk (depth + 1) parents
          end
        ) commits
    in
    let* () = walk 0 [head] in
    Lwt.return (List.rev !results)

(* Check if a commit message has [experience] tag *)
let is_experience_commit ~message =
  let prefix = "[experience]" in
  String.length message >= String.length prefix &&
  String.sub message 0 (String.length prefix) = prefix

(* Parse experience metadata from commit message *)
let parse_experience_message message =
  let lines = String.split_on_char '\n' message in
  match lines with
  | [] -> None
  | first :: rest ->
    let prefix = "[experience] " in
    if String.length first >= String.length prefix &&
       String.sub first 0 (String.length prefix) = prefix then
      let label = String.sub first (String.length prefix)
        (String.length first - String.length prefix) in
      let kvs = List.filter_map (fun line ->
        match String.index_opt line ':' with
        | Some idx ->
          let key = String.trim (String.sub line 0 idx) in
          let value = String.trim (String.sub line (idx + 1)
            (String.length line - idx - 1)) in
          Some (key, value)
        | None -> None
      ) rest in
      let get key = List.assoc_opt key kvs |> Option.value ~default:"" in
      Some (label, get "intent", get "session", get "uuids")
    else None

(* List all files in a tree recursively *)
let rec list_tree_files ~prefix tree =
  let* entries = Store.Tree.list tree [] in
  let* nested = Lwt_list.map_s (fun (step, subtree) ->
    let path = if prefix = "" then step else prefix ^ "/" ^ step in
    let* kind = Store.Tree.kind subtree [] in
    match kind with
    | Some `Contents -> Lwt.return [path]
    | Some `Node -> list_tree_files ~prefix:path subtree
    | None -> Lwt.return []
  ) entries in
  Lwt.return (List.concat nested)

(* Build experience from a commit *)
let experience_of_commit ~repo ~sha =
  let* (message, _author, date, parents) = commit_info ~repo ~sha in
  let parent_sha = match parents with p :: _ -> p | [] -> "" in
  let* diff = if parent_sha <> "" then
    diff_between ~repo ~sha1:parent_sha ~sha2:sha
  else Lwt.return "" in
  let* files = if parent_sha <> "" then
    changed_files ~repo ~sha1:parent_sha ~sha2:sha
  else begin
    (* Initial commit: list all files in the tree *)
    let* commit_opt = find_commit ~repo sha in
    match commit_opt with
    | None -> Lwt.return []
    | Some commit ->
      let tree = Store.Commit.tree commit in
      list_tree_files ~prefix:"" tree
  end in
  match parse_experience_message message with
  | Some (label, intent, session_id, uuids_str) ->
    let uuids = String.split_on_char ',' uuids_str
      |> List.filter (fun s -> String.length s > 0) in
    Lwt.return_some Types.{
      id = sha;
      label;
      intent;
      session_id;
      interaction_uuids = uuids;
      head_sha = sha;
      commit_sha = sha;
      commit_message = message;
      parent_sha;
      diff;
      branch = "";
      files_changed = files;
      timestamp = Int64.to_float date;
      reverted = false;
    }
  | None ->
    (* Non-experience commit — use commit message as label *)
    Lwt.return_some Types.{
      id = sha;
      label = (let first_line = match String.split_on_char '\n' message with
        | l :: _ -> l | [] -> "" in
        if String.length first_line > 80
        then String.sub first_line 0 80 ^ "..."
        else first_line);
      intent = message;
      session_id = "";
      interaction_uuids = [];
      head_sha = sha;
      commit_sha = sha;
      commit_message = message;
      parent_sha;
      diff;
      branch = "";
      files_changed = files;
      timestamp = Int64.to_float date;
      reverted = false;
    }

(* List all experience-tagged commits *)
let list_experience_commits ~repo =
  let* results = walk_history ~repo
    ~filter:(fun ~sha:_ ~message -> is_experience_commit ~message) () in
  Lwt_list.filter_map_s (fun (sha, _commit) ->
    experience_of_commit ~repo ~sha
  ) results

(* Find merge commits since a given time *)
let find_merge_commits ~repo ~since =
  walk_history ~repo ~filter:(fun ~sha:_ ~message:_ -> true) ()
  |> Lwt.map (List.filter (fun (_sha, commit) ->
    let parents = Store.Commit.parents commit in
    let info = Store.Commit.info commit in
    let date = Int64.to_float (Store.Info.date info) in
    List.length parents > 1 && date >= since
  ))

(* Walk all commits since a date — for --init *)
let walk_all_commits ~repo ~since =
  let* results = walk_history ~repo
    ~filter:(fun ~sha:_ ~message:_ -> true) () in
  let filtered = List.filter (fun (_sha, commit) ->
    let info = Store.Commit.info commit in
    let date = Int64.to_float (Store.Info.date info) in
    date >= since
  ) results in
  (* Return oldest first *)
  Lwt.return (List.rev filtered)

(* Find commits that changed a specific file *)
let find_commits_changing_file ~repo ~filepath =
  let* results = walk_history ~repo
    ~filter:(fun ~sha:_ ~message:_ -> true) () in
  Lwt_list.filter_map_s (fun (sha, commit) ->
    let parent_keys = Store.Commit.parents commit in
    match parent_keys with
    | [] -> Lwt.return_none
    | parent_key :: _ ->
      let* parent_opt = Store.Commit.of_key repo parent_key in
      match parent_opt with
      | None -> Lwt.return_none
      | Some parent ->
        let tree1 = Store.Commit.tree parent in
        let tree2 = Store.Commit.tree commit in
        let+ diffs = Store.Tree.diff tree1 tree2 in
        let path_parts = String.split_on_char '/' filepath in
        let touches_file = List.exists (fun (path, _change) ->
          path = path_parts
        ) diffs in
        if touches_file then Some sha else None
  ) results

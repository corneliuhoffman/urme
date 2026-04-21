(* MCP tool handlers — V2 SQLite + pure analysis engine. No ChromaDB.

   The six tools run against .urme/db.sqlite (populated by `urme init`)
   and use Git_link's pure analysis functions for per-edit provenance. *)

open Lwt.Syntax
open Urme_engine.Git_link_types

module D = Urme_store.Db
module Schema = Urme_store.Schema
module Search = Urme_search.Search
module S = Sqlite3

type state = {
  project_dir : string;
  mutable db : S.db option;
  mutable repo : Urme_store.Project_store.Store.Repo.t option;
  mutable edits : edit list option;
  mutable branch_label : (string, string) Hashtbl.t option;
}

let create_state ~project_dir =
  let project_dir =
    if project_dir = "." || project_dir = "./" then Sys.getcwd ()
    else if Filename.is_relative project_dir
    then Filename.concat (Sys.getcwd ()) project_dir
    else project_dir
  in
  { project_dir; db = None; repo = None; edits = None; branch_label = None }

let ensure_db st =
  match st.db with
  | Some db -> db
  | None ->
    let db = Schema.open_or_create ~project_dir:st.project_dir in
    st.db <- Some db;
    db

let ensure_repo st =
  match st.repo with
  | Some r -> Lwt.return r
  | None ->
    let* r = Urme_store.Project_store.open_repo ~project_dir:st.project_dir in
    st.repo <- Some r;
    Lwt.return r

let ensure_edits st =
  match st.edits with
  | Some e -> Lwt.return e
  | None ->
    let pool = Domainslib.Task.setup_pool ~num_domains:4 () in
    let e = Urme_engine.Edit_extract.edits_of_sessions ~pool
        ~project_dir:st.project_dir in
    Domainslib.Task.teardown_pool pool;
    st.edits <- Some e;
    Lwt.return e

let ensure_branch_label st =
  match st.branch_label with
  | Some bl -> Lwt.return bl
  | None ->
    let* bl = Urme_engine.Branch_topo.label_commits ~cwd:st.project_dir in
    st.branch_label <- Some bl;
    Lwt.return bl

(* ---------- Result formatters ---------- *)

let text_result text =
  `Assoc [
    "content", `List [
      `Assoc ["type", `String "text"; "text", `String text];
    ];
  ]

let json_result j = text_result (Yojson.Safe.pretty_to_string j)

let rec provenance_to_json = function
  | DirectEdit e ->
    `Assoc [
      "type", `String "claude_edit";
      "edit_key", `String e.edit_key;
      "file", `String e.file_base;
      "session_id", `String e.session_id;
      "turn_idx", `Int e.turn_idx;
      "entry_idx", `Int e.entry_idx;
      "timestamp", `Float e.timestamp;
      "old_string", `String (if String.length e.old_string > 200
        then String.sub e.old_string 0 200 ^ "..." else e.old_string);
      "new_string", `String (if String.length e.new_string > 200
        then String.sub e.new_string 0 200 ^ "..." else e.new_string);
    ]
  | Incoming (provs, branch) ->
    let items = List.filter_map (fun p -> match p with
      | DirectEdit _ -> Some (provenance_to_json p) | _ -> None) provs in
    `Assoc ["type", `String "incoming_merge";
            "branch", `String branch; "items", `List items]
  | HumanEdit (e, human_text) ->
    `Assoc [
      "type", `String "human_edit";
      "edit_key", `String e.edit_key;
      "file", `String e.file_base;
      "claude_new_string", `String (if String.length e.new_string > 200
        then String.sub e.new_string 0 200 ^ "..." else e.new_string);
      "human_version", `String (if String.length human_text > 200
        then String.sub human_text 0 200 ^ "..." else human_text);
    ]
  | ConflictChoice _ -> `Assoc ["type", `String "conflict_choice"]
  | ConflictResolution _ -> `Assoc ["type", `String "conflict_resolution"]
  | Unexplained msg -> `Assoc ["type", `String "unexplained"; "message", `String msg]

let decomposition_to_json (d : decomposition) =
  let claude_edits = List.filter_map (fun item -> match item with
    | DirectEdit _ -> Some (provenance_to_json item)
    | Incoming _ -> Some (provenance_to_json item)
    | _ -> None) d.items in
  let warnings = List.filter_map (fun item -> match item with
    | Unexplained msg -> Some (`String msg) | _ -> None) d.items in
  `Assoc [
    "commit_sha", `String d.commit_sha;
    "file", `String d.file;
    "claude_edits", `List claude_edits;
    "warnings", `List warnings;
    "n_edits", `Int (List.length claude_edits);
  ]

let hit_to_json (h : Search.hit) =
  `Assoc [
    "step_id", `Int h.step_id;
    "session_id",
      (match h.session_id with Some s -> `String s | None -> `Null);
    "turn_index", `Int h.turn_index;
    "timestamp", `Float h.timestamp;
    "summary", `String h.summary;
    "tags", `String h.tags;
    "prompt_text", `String h.prompt_text;
    "files_touched", (try Yojson.Safe.from_string h.files_touched
                      with _ -> `List []);
    "commit_before",
      (match h.commit_before with Some s -> `String s | None -> `Null);
    "commit_after",
      (match h.commit_after with Some s -> `String s | None -> `Null);
    "score", `Float h.score;
  ]

(* Generic "row from steps" returned as JSON, shared between file_history,
   commit_links, etc. Columns must match the SELECT we use below. *)
let step_row_to_json cols =
  `Assoc [
    "step_id", `Int (D.data_to_int cols.(0));
    "session_id",
      (match D.data_to_string_opt cols.(1) with
       | Some s -> `String s | None -> `Null);
    "turn_index", `Int (D.data_to_int cols.(2));
    "timestamp", `Float (D.data_to_float cols.(3));
    "summary", `String (D.data_to_string cols.(4));
    "tags", `String (D.data_to_string cols.(5));
    "prompt_text", `String (D.data_to_string cols.(6));
    "files_touched", (try Yojson.Safe.from_string (D.data_to_string cols.(7))
                      with _ -> `List []);
    "commit_before",
      (match D.data_to_string_opt cols.(8) with
       | Some s -> `String s | None -> `Null);
    "commit_after",
      (match D.data_to_string_opt cols.(9) with
       | Some s -> `String s | None -> `Null);
  ]

let step_select_cols =
  "s.id, s.session_id, s.turn_index, s.timestamp, \
   COALESCE(s.summary,''), COALESCE(s.tags,''), \
   COALESCE(s.prompt_text,''), COALESCE(s.files_touched,'[]'), \
   s.commit_before, s.commit_after"

(* ---------- Tool implementations ---------- *)

let handle_search_history st args =
  let open Yojson.Safe.Util in
  let query = args |> member "query" |> to_string in
  let n = try args |> member "n" |> to_int with _ -> 5 in
  let db = ensure_db st in
  let hits = Search.run_with_fallback ~db ~limit:n query in
  Lwt.return (json_result (`Assoc [
    "query", `String query;
    "n_results", `Int (List.length hits);
    "results", `List (List.map hit_to_json hits);
  ]))

let handle_file_history st args =
  let open Yojson.Safe.Util in
  let file_path = args |> member "file_path" |> to_string in
  let basename = Filename.basename file_path in
  let db = ensure_db st in
  (* Step-level view: turns that touched the file, ordered by timestamp. *)
  let sql =
    Printf.sprintf
      "SELECT %s FROM steps s \
       WHERE s.files_touched LIKE ? OR s.files_touched LIKE ? \
       ORDER BY s.timestamp ASC"
      step_select_cols
  in
  let rows = D.query_list db sql
    [S.Data.TEXT ("%\"" ^ basename ^ "\"%");
     S.Data.TEXT ("%" ^ file_path ^ "%")]
    ~f:step_row_to_json in
  (* Per-edit provenance via diff_match for each commit that touched the
     file — this is the fine-grained view the old Chroma-backed handler
     used to return. *)
  let* edits = ensure_edits st in
  let* branch_label = ensure_branch_label st in
  let* repo = ensure_repo st in
  let* decompositions = Urme_engine.Git_link.file_history
      ~project_dir:st.project_dir ~file_path
      ~edits ~branch_label ~repo in
  Lwt.return (json_result (`Assoc [
    "file_path", `String file_path;
    "n_steps", `Int (List.length rows);
    "steps", `List rows;
    "n_commits", `Int (List.length decompositions);
    "commits", `List (List.map decomposition_to_json decompositions);
  ]))

let handle_region_blame st args =
  let open Yojson.Safe.Util in
  let file_path = args |> member "file_path" |> to_string in
  let start_line = args |> member "start_line" |> to_int in
  let end_line = args |> member "end_line" |> to_int in
  let* blame_lines = Lwt.catch (fun () ->
    Urme_git.Ops.blame ~cwd:st.project_dir
      ~line_range:(start_line, end_line) ~filepath:file_path ()
  ) (fun _ -> Lwt.return []) in
  let blame_json = List.map (fun (sha, line_num, content) ->
    `Assoc [
      "sha", `String (if String.length sha >= 8 then String.sub sha 0 8 else sha);
      "line", `Int line_num;
      "content", `String content;
    ]
  ) blame_lines in
  let* edits = ensure_edits st in
  let* branch_label = ensure_branch_label st in
  let* repo = ensure_repo st in
  let* decompositions = Urme_engine.Git_link.region_history
      ~project_dir:st.project_dir ~path:file_path
      ~start_line ~end_line ~edits ~branch_label ~repo in
  Lwt.return (json_result (`Assoc [
    "file_path", `String file_path;
    "lines", `String (Printf.sprintf "%d-%d" start_line end_line);
    "blame", `List blame_json;
    "claude_history",
      `List (List.map decomposition_to_json decompositions);
  ]))

(* A commit's "explanation" = raw diff + every step whose commit_after
   matches, ideally filtered to the file at hand. *)
let handle_explain_change st args =
  let open Yojson.Safe.Util in
  let sha = args |> member "commit_sha" |> to_string in
  let file_path = args |> member "file_path" |> to_string in
  let basename = Filename.basename file_path in
  let* diff = Lwt.catch (fun () ->
    Urme_git.Ops.run_git ~cwd:st.project_dir
      ["diff"; sha ^ "^"; sha; "--"; file_path]
  ) (fun _ -> Lwt.return "") in
  let db = ensure_db st in
  let sql =
    Printf.sprintf
      "SELECT %s FROM steps s \
       WHERE (s.commit_after LIKE ? OR s.commit_before LIKE ?) \
         AND s.files_touched LIKE ? \
       ORDER BY s.timestamp ASC"
      step_select_cols
  in
  let sha_like = sha ^ "%" in
  let rows = D.query_list db sql
    [S.Data.TEXT sha_like;
     S.Data.TEXT sha_like;
     S.Data.TEXT ("%\"" ^ basename ^ "\"%")]
    ~f:step_row_to_json in
  Lwt.return (json_result (`Assoc [
    "commit_sha", `String sha;
    "file", `String file_path;
    "diff", `String (if String.length diff > 5000
      then String.sub diff 0 5000 ^ "\n... (truncated)" else diff);
    "explanatory_steps", `List rows;
    "n_steps", `Int (List.length rows);
  ]))

let handle_commit_links st args =
  let open Yojson.Safe.Util in
  let sha = args |> member "commit_sha" |> to_string in
  let db = ensure_db st in
  let sql =
    Printf.sprintf
      "SELECT %s FROM steps s \
       WHERE s.commit_after LIKE ? OR s.commit_before LIKE ? \
       ORDER BY s.timestamp ASC"
      step_select_cols
  in
  let sha_like = sha ^ "%" in
  let rows = D.query_list db sql
    [S.Data.TEXT sha_like; S.Data.TEXT sha_like]
    ~f:step_row_to_json in
  Lwt.return (json_result (`Assoc [
    "commit_sha", `String sha;
    "n_steps", `Int (List.length rows);
    "steps", `List rows;
  ]))

let handle_search_by_file st args =
  let open Yojson.Safe.Util in
  let file_path = args |> member "file_path" |> to_string in
  let n = try args |> member "n" |> to_int with _ -> 10 in
  let basename = Filename.basename file_path in
  let db = ensure_db st in
  (* First pass: exact files_touched containment; then fall back to FTS5
     which indexes the basename if it appears in summary/tags/prompt. *)
  let sql =
    Printf.sprintf
      "SELECT %s FROM steps s \
       WHERE s.files_touched LIKE ? \
       ORDER BY s.timestamp DESC LIMIT ?"
      step_select_cols
  in
  let rows = D.query_list db sql
    [S.Data.TEXT ("%\"" ^ basename ^ "\"%");
     S.Data.INT (Int64.of_int n)]
    ~f:step_row_to_json in
  let hits_json =
    if List.length rows >= n then rows
    else
      let fts_hits = Search.run ~db ~limit:n basename in
      rows @ List.map hit_to_json fts_hits
  in
  Lwt.return (json_result (`Assoc [
    "file", `String basename;
    "n_results", `Int (List.length hits_json);
    "results", `List hits_json;
  ]))

(* ---------- Push to TUI (unchanged) ---------- *)

let send_to_tui ~project_dir ~msg =
  let socket_path = Urme_core.Paths.tui_socket_path ~project_dir in
  if not (Sys.file_exists socket_path) then Lwt.return_unit
  else
    Lwt.catch (fun () ->
      let socket = Lwt_unix.socket Unix.PF_UNIX Unix.SOCK_STREAM 0 in
      let addr = Unix.ADDR_UNIX socket_path in
      let* () = Lwt_unix.connect socket addr in
      let oc = Lwt_io.of_fd ~mode:Lwt_io.Output socket in
      let* () = Lwt_io.write_line oc (Yojson.Safe.to_string msg) in
      let* () = Lwt_io.flush oc in
      let ic = Lwt_io.of_fd ~mode:Lwt_io.Input socket in
      let* _ack = Lwt_io.read_line ic in
      Lwt_unix.close socket
    ) (fun _exn -> Lwt.return_unit)

(* ---------- Dispatch ---------- *)

let dispatch st name args =
  let* result = match name with
    | "search_history" -> handle_search_history st args
    | "file_history"   -> handle_file_history st args
    | "region_blame"   -> handle_region_blame st args
    | "explain_change" -> handle_explain_change st args
    | "commit_links"   -> handle_commit_links st args
    | "search_by_file" -> handle_search_by_file st args
    | _ -> Lwt.return (text_result (Printf.sprintf "Unknown tool: %s" name))
  in
  let msg = `Assoc ["type", `String name; "result", result] in
  Lwt.async (fun () -> send_to_tui ~project_dir:st.project_dir ~msg);
  Lwt.return result

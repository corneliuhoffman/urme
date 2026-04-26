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

(* ---------- Push to the running URME TUI (Unix socket) ---------- *)

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

(* ---------- Tool implementations ---------- *)

(* [handle_search_history]

   Two-sink delivery:

   - Push the full enriched payload (hits + assistant_text for top 5)
     to the running URME TUI over a Unix socket. URME shows results
     live; zero context cost on Claude.
   - Return a SLIM summary to the calling Claude: step_id, session
     prefix, turn, date, one-line summary per hit. Enough for Claude
     to rank and cite, nothing more.

   If the user needs the full text of a single turn for synthesis,
   Claude calls [get_turn] on it — pay-per-turn instead of dumping
   everything. *)
let handle_search_history st args =
  let open Yojson.Safe.Util in
  let parse_iso_date s =
    try
      Scanf.sscanf s "%4d-%2d-%2d" (fun y m d ->
        let dim = [|31;28;31;30;31;30;31;31;30;31;30;31|] in
        let leap yr =
          (yr mod 4 = 0 && yr mod 100 <> 0) || yr mod 400 = 0 in
        let rec yd acc yr =
          if yr <= 1970 then acc
          else yd (acc + if leap (yr - 1) then 366 else 365) (yr - 1) in
        let md = ref 0 in
        for mi = 0 to m - 2 do
          md := !md + dim.(mi);
          if mi = 1 && leap y then md := !md + 1
        done;
        Some (Float.of_int (((yd 0 y + !md + d - 1) * 86400))))
    with _ -> None in
  let fts_terms =
    try args |> member "fts_terms" |> to_string with _ -> "" in
  let query_fallback =
    try args |> member "query" |> to_string with _ -> "" in
  let limit = try args |> member "limit" |> to_int with _ -> 20 in
  let order_by =
    match (try args |> member "order_by" |> to_string with _ -> "relevance")
          |> String.lowercase_ascii with
    | "earliest" -> Urme_claude.Prompts.Earliest
    | "latest"   -> Urme_claude.Prompts.Latest
    | _          -> Urme_claude.Prompts.Relevance in
  let require_summary =
    try args |> member "require_summary" |> to_bool with _ -> true in
  let date_opt key =
    match args |> member key with
    | `String d -> parse_iso_date d
    | _ -> None in
  let after  = date_opt "after" in
  let before = date_opt "before" in
  let db = ensure_db st in
  let hits =
    if fts_terms <> "" then
      let spec : Urme_claude.Prompts.query_spec = {
        fts_terms; order_by;
        limit = max 10 (min 200 limit);
        require_summary; after; before } in
      let hs = Search.run_spec ~db spec in
      if hs = [] && query_fallback <> "" then
        Search.run_with_fallback ~db ~limit query_fallback
      else hs
    else if query_fallback <> "" then
      Search.run_with_fallback ~db ~limit query_fallback
    else [] in
  let full_text_count = 5 in
  let iso_of_ts ts =
    let tm = Unix.gmtime ts in
    Printf.sprintf "%04d-%02d-%02d"
      (tm.Unix.tm_year + 1900) (tm.Unix.tm_mon + 1) tm.Unix.tm_mday in
  let short_sid s =
    if String.length s >= 8 then String.sub s 0 8 else s in
  (* Enriched response for Claude: slim fields for every hit, plus full
     prompt_text + assistant_text for the top N. Calling-Claude has a
     wide context window, no clamping. *)
  let results =
    List.mapi (fun i (h : Search.hit) ->
      let sid_opt = match h.session_id with
        | Some s -> `String (short_sid s)
        | None -> `Null in
      let base = [
        "step_id", `Int h.step_id;
        "session", sid_opt;
        "session_id",
          (match h.session_id with Some s -> `String s | None -> `Null);
        "turn", `Int h.turn_index;
        "date", `String (iso_of_ts h.timestamp);
        "summary", `String h.summary;
      ] in
      if i < full_text_count then
        let assistant_text = match h.session_id with
          | Some sid ->
            Search.fetch_assistant_text
              ~project_dir:st.project_dir
              ~session_id:sid ~turn_index:h.turn_index
          | None -> "" in
        `Assoc (base @ [
          "prompt_text", `String h.prompt_text;
          "assistant_text", `String assistant_text;
        ])
      else `Assoc base
    ) hits in
  Lwt.return (json_result (`Assoc [
    "fts_terms", `String fts_terms;
    "query", `String query_fallback;
    "n_results", `Int (List.length results);
    "results", `List results;
    "note",
      `String
        "Top 5 results include full prompt_text + assistant_text as \
         evidence. Rank them yourself: drop tangential, lexical-only, \
         or opposite-direction hits (if the user asks when a feature \
         was ADDED, drop turns about REMOVING it, and vice versa). \
         Prefer 0 or 1 clear citation over several weak ones. After \
         producing your one-or-two-sentence answer, call \
         `push_synthesis` with the answer text and the cited \
         {session_id, turn} pairs so the running URME TUI can show \
         the user your conclusion and the evidence behind it.";
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

(* Fetch one turn's full prompt + assistant text. Cheap, pay-per-turn
   alternative to dumping everything in [search_history]. *)
let handle_get_turn st args =
  let open Yojson.Safe.Util in
  let session_id = args |> member "session_id" |> to_string in
  let turn_index = args |> member "turn_index" |> to_int in
  let assistant_text =
    Search.fetch_assistant_text
      ~project_dir:st.project_dir ~session_id ~turn_index in
  (* Also pull the user prompt from the steps row. *)
  let db = ensure_db st in
  let sql =
    "SELECT COALESCE(prompt_text,''), COALESCE(summary,''), timestamp \
     FROM steps WHERE session_id = ? AND turn_index = ? LIMIT 1" in
  let row =
    D.query_list db sql
      [S.Data.TEXT session_id; S.Data.INT (Int64.of_int turn_index)]
      ~f:(fun cols ->
        `Assoc [
          "prompt_text", `String (D.data_to_string cols.(0));
          "summary",     `String (D.data_to_string cols.(1));
          "timestamp",   `Float  (D.data_to_float  cols.(2));
        ]) in
  let base = match row with
    | x :: _ -> x
    | [] -> `Assoc [
      "prompt_text", `String "";
      "summary", `String "";
      "timestamp", `Float 0.;
    ] in
  let merged = match base with
    | `Assoc fs ->
      `Assoc (fs @ [
        "session_id", `String session_id;
        "turn_index", `Int turn_index;
        "assistant_text", `String assistant_text;
      ])
    | other -> other in
  Lwt.return (json_result merged)

(* Push the calling Claude's final synthesis to the running URME TUI,
   along with the full text of the cited turns (so the user can verify
   the conclusion against the evidence). This is the only socket push
   the search pipeline makes — `search_history` itself is silent. *)
(* Find substring [needle] in [s], starting at [from]. Returns the
   index of the first match or None. *)
let find_substring_from s ~from needle =
  let nlen = String.length needle in
  let slen = String.length s in
  if nlen = 0 || from < 0 || from + nlen > slen then None
  else
    let rec scan i =
      if i + nlen > slen then None
      else if String.sub s i nlen = needle then Some i
      else scan (i + 1)
    in scan from

(* Walk forward from [start] (which must point at '['), tracking bracket
   depth and string state, return the index just past the matching ']'.
   Used to extract a complete JSON array from inside a free-text blob. *)
let json_array_end s start =
  let len = String.length s in
  if start >= len || s.[start] <> '[' then None
  else
    let depth = ref 0 in
    let in_str = ref false in
    let escape = ref false in
    let i = ref start in
    let result = ref None in
    while !result = None && !i < len do
      let c = s.[!i] in
      (if !in_str then
         if !escape then escape := false
         else if c = '\\' then escape := true
         else if c = '"' then in_str := false
         else ()
       else
         match c with
         | '"' -> in_str := true
         | '[' -> incr depth
         | ']' ->
           decr depth;
           if !depth = 0 then result := Some (!i + 1)
         | _ -> ());
      incr i
    done;
    !result

(* Salvage a [cited] array that the model embedded in the synthesis
   string as <parameter name="cited">[...]</parameter> instead of
   passing it as a sibling JSON argument. Returns (cleaned_synthesis,
   cited_json) when found, else (synthesis, `Null). *)
let salvage_embedded_cited synthesis =
  let tag = "<parameter name=\"cited\">" in
  match find_substring_from synthesis ~from:0 tag with
  | None -> (synthesis, `Null)
  | Some tag_pos ->
    let arr_start = tag_pos + String.length tag in
    (* Skip whitespace before the '['. *)
    let len = String.length synthesis in
    let rec skip_ws i =
      if i < len && (synthesis.[i] = ' ' || synthesis.[i] = '\n'
                     || synthesis.[i] = '\t' || synthesis.[i] = '\r')
      then skip_ws (i + 1) else i
    in
    let arr_start = skip_ws arr_start in
    match json_array_end synthesis arr_start with
    | None -> (synthesis, `Null)
    | Some arr_end ->
      let arr_text = String.sub synthesis arr_start (arr_end - arr_start) in
      (match try Some (Yojson.Safe.from_string arr_text) with _ -> None with
       | Some json ->
         (* Strip from tag_pos to either end of </parameter> or arr_end. *)
         let close_tag = "</parameter>" in
         let strip_end =
           match find_substring_from synthesis ~from:arr_end close_tag with
           | Some p -> p + String.length close_tag
           | None -> arr_end in
         let before = String.sub synthesis 0 tag_pos in
         let after = String.sub synthesis strip_end (len - strip_end) in
         (String.trim (before ^ after), json)
       | None -> (synthesis, `Null))

let handle_push_synthesis st args =
  let open Yojson.Safe.Util in
  let synthesis_raw =
    match args |> member "synthesis" with
    | `String s when String.trim s <> "" -> s
    | `String _ ->
      failwith "push_synthesis: 'synthesis' must be a non-empty string"
    | `Null ->
      failwith "push_synthesis: missing required field 'synthesis'"
    | _ ->
      failwith "push_synthesis: 'synthesis' must be a string" in
  let cited_arg = args |> member "cited" in
  (* If the model leaked the cited array into the synthesis text as
     <parameter name="cited">[...]</parameter>, recover it and strip
     the XML from the displayed synthesis. *)
  let synthesis, cited_arg =
    match cited_arg with
    | `List (_ :: _) -> (synthesis_raw, cited_arg)
    | _ ->
      let cleaned, salvaged = salvage_embedded_cited synthesis_raw in
      (cleaned, (match salvaged with `Null -> cited_arg | j -> j)) in
  let cited =
    match cited_arg with
    | `Null -> []
    | `List xs ->
      List.mapi (fun i j ->
        let sid =
          match j |> member "session_id" with
          | `String s when s <> "" -> s
          | _ ->
            failwith (Printf.sprintf
              "push_synthesis: cited[%d].session_id must be a non-empty string"
              i) in
        let turn =
          match j |> member "turn_index" with
          | `Int n -> n
          | _ ->
            failwith (Printf.sprintf
              "push_synthesis: cited[%d].turn_index must be an integer" i) in
        (sid, turn)) xs
    | _ ->
      failwith "push_synthesis: 'cited' must be a JSON array" in
  let cited_results =
    List.map (fun (session_id, turn_index) ->
      let assistant_text =
        Search.fetch_assistant_text
          ~project_dir:st.project_dir ~session_id ~turn_index in
      let db = ensure_db st in
      let sql =
        "SELECT COALESCE(prompt_text,''), COALESCE(summary,''), timestamp \
         FROM steps WHERE session_id = ? AND turn_index = ? LIMIT 1" in
      let row =
        D.query_list db sql
          [S.Data.TEXT session_id; S.Data.INT (Int64.of_int turn_index)]
          ~f:(fun cols ->
            (D.data_to_string cols.(0),
             D.data_to_string cols.(1),
             D.data_to_float  cols.(2))) in
      let prompt_text, summary, timestamp = match row with
        | x :: _ -> x
        | [] -> ("", "", 0.) in
      `Assoc [
        "session_id", `String session_id;
        "turn_index", `Int turn_index;
        "timestamp", `Float timestamp;
        "summary", `String summary;
        "prompt_text", `String prompt_text;
        "assistant_text", `String assistant_text;
      ]) cited in
  let tui_msg = `Assoc [
    "type", `String "synthesis";
    "synthesis", `String synthesis;
    "cited", `List cited_results;
  ] in
  let* () = send_to_tui ~project_dir:st.project_dir ~msg:tui_msg in
  Lwt.return (text_result
    (Printf.sprintf "Pushed synthesis (%d cited) to TUI."
       (List.length cited_results)))

(* ---------- Dispatch ---------- *)

let dispatch st name args =
  match name with
  | "search_history" -> handle_search_history st args
  | "push_synthesis" -> handle_push_synthesis st args
  | "get_turn"       -> handle_get_turn st args
  | "file_history"   -> handle_file_history st args
  | "region_blame"   -> handle_region_blame st args
  | "explain_change" -> handle_explain_change st args
  | "commit_links"   -> handle_commit_links st args
  | "search_by_file" -> handle_search_by_file st args
  | _ -> Lwt.return (text_result (Printf.sprintf "Unknown tool: %s" name))

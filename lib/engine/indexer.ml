(* Index a Claude Code JSONL session into the urme V2 SQLite store.

   For each real-user → assistant exchange (an "interaction" in the existing
   jsonl_reader terminology) we write one [steps] row with deterministic
   metadata. Summary/tags stay NULL — the Claude CLI summarisation pass
   (task #3) fills them later.

   Git correlation: we walk git log once per session to get (sha, ts) pairs,
   then for each turn stamp the latest commit ≤ turn.ts as commit_before and
   the earliest commit > turn.ts as commit_after. Branch-filtered when the
   interaction records one. *)

module D = Urme_store.Db
module Schema = Urme_store.Schema
module S = Sqlite3
module Jsonl = Urme_search.Jsonl_reader
open Urme_core.Types

(* ---------- Per-turn metadata extraction ---------- *)

(* Read raw JSONL range into yojson values. *)
let jsons_in_range ~filepath ~line_start ~line_end =
  let lines = Jsonl.read_all_lines filepath in
  List.filteri (fun i _ -> i >= line_start && i <= line_end) lines
  |> List.filter_map (fun l -> try Some (Yojson.Safe.from_string l) with _ -> None)

(* Raw text of the JSONL records for this turn, joined with '\n'. We
   persist this in [steps.turn_json] so the TUI can render the turn's
   full body even after Claude Code prunes the source JSONL. *)
let raw_lines_in_range ~filepath ~line_start ~line_end =
  let lines = Jsonl.read_all_lines filepath in
  List.filteri (fun i _ -> i >= line_start && i <= line_end) lines
  |> String.concat "\n"

(* Flatten the assistant's [text] blocks across this turn's JSONL
   records. Concatenated with blank lines between blocks — this is the
   column FTS5 tokenises for "search across the assistant's reasoning"
   hits. *)
let flatten_assistant_text jsons =
  let open Yojson.Safe.Util in
  let out = Buffer.create 256 in
  List.iter (fun j ->
    match j |> member "type" |> to_string_option with
    | Some "assistant" ->
      let blocks =
        try j |> member "message" |> member "content" |> to_list
        with _ -> [] in
      List.iter (fun b ->
        match b |> member "type" |> to_string_option with
        | Some "text" ->
          (match b |> member "text" |> to_string_option with
           | Some t when t <> "" ->
             if Buffer.length out > 0 then Buffer.add_char out '\n';
             Buffer.add_string out t
           | _ -> ())
        | _ -> ()) blocks
    | _ -> ()) jsons;
  Buffer.contents out

(* Pull thinking blocks from assistant content blocks in this range.
   Returned in original order (one string per block). *)
let extract_thinking jsons =
  let open Yojson.Safe.Util in
  List.concat_map (fun json ->
    match json |> member "type" |> to_string_option with
    | Some "assistant" ->
      let blocks = json |> member "message" |> member "content"
        |> (fun j -> try to_list j with _ -> []) in
      List.filter_map (fun block ->
        match block |> member "type" |> to_string_option with
        | Some "thinking" ->
          let t = block |> member "thinking" |> to_string_option
                  |> Option.value ~default:"" in
          if t = "" then None else Some t
        | _ -> None
      ) blocks
    | _ -> []
  ) jsons

(* Pull Bash commands from tool_use blocks in an interaction's range. *)
let extract_commands jsons =
  let open Yojson.Safe.Util in
  List.concat_map (fun json ->
    match json |> member "type" |> to_string_option with
    | Some "assistant" ->
      let blocks = json |> member "message" |> member "content"
        |> (fun j -> try to_list j with _ -> []) in
      List.filter_map (fun block ->
        match block |> member "type" |> to_string_option with
        | Some "tool_use" ->
          let name = block |> member "name" |> to_string_option
            |> Option.value ~default:"" in
          if name = "Bash" then
            block |> member "input" |> member "command" |> to_string_option
          else None
        | _ -> None
      ) blocks
    | _ -> []
  ) jsons

(* Sum usage.input_tokens / output_tokens across assistant messages. *)
let sum_tokens jsons =
  let open Yojson.Safe.Util in
  List.fold_left (fun (tin, tout) json ->
    match json |> member "type" |> to_string_option with
    | Some "assistant" ->
      let usage = json |> member "message" |> member "usage" in
      let i = usage |> member "input_tokens" |> to_int_option |> Option.value ~default:0 in
      let o = usage |> member "output_tokens" |> to_int_option |> Option.value ~default:0 in
      (tin + i, tout + o)
    | _ -> (tin, tout)
  ) (0, 0) jsons

(* ---------- Git correlation ---------- *)

(* (sha, unix_ts) pairs, newest first, optionally filtered to a branch. *)
let load_branch_commits ~project_dir ~branch =
  let argv =
    let base = ["-C"; project_dir; "log"; "--format=%H %ct"; "--max-count=5000"] in
    if branch = "" then Array.of_list ("git" :: base)
    else Array.of_list ("git" :: base @ [branch])
  in
  let rd, wr = Unix.pipe ~cloexec:true () in
  let devnull = Unix.openfile "/dev/null" [Unix.O_WRONLY; Unix.O_CLOEXEC] 0 in
  let safe_close fd = try Unix.close fd with _ -> () in
  let pid =
    try Unix.create_process "git" argv Unix.stdin wr devnull
    with e -> safe_close rd; safe_close wr; safe_close devnull; raise e
  in
  safe_close wr;
  safe_close devnull;
  let ic = Unix.in_channel_of_descr rd in
  (* EINTR-safe waitpid — TUI signal handlers (SIGWINCH) can interrupt. *)
  let rec reap () =
    try ignore (Unix.waitpid [] pid)
    with
    | Unix.Unix_error (Unix.EINTR, _, _) -> reap ()
    | _ -> ()
  in
  (* Wrap read/close/reap so a mid-read exception still reaps the child
     and closes the fd. Without this, any failure leaks a zombie + fd. *)
  Fun.protect ~finally:(fun () ->
    (try close_in ic with _ -> ());
    reap ())
    (fun () ->
      let rec loop acc =
        match input_line ic with
        | line ->
          let acc' =
            match String.index_opt line ' ' with
            | Some i ->
              let sha = String.sub line 0 i in
              let ts_s = String.sub line (i + 1) (String.length line - i - 1) in
              (try (sha, float_of_string ts_s) :: acc
               with _ -> acc)
            | None -> acc
          in
          loop acc'
        | exception End_of_file -> acc
      in
      (* walk_log order is newest-first; we reverse to append oldest-first. *)
      List.rev (loop []))

(* Given a commits list sorted oldest-first and a turn timestamp,
   return (commit_before, commit_after). *)
let stamp_bounds commits turn_ts =
  let before =
    List.fold_left (fun acc (sha, ts) ->
      if ts <= turn_ts then Some sha else acc
    ) None commits in
  let after =
    List.find_map (fun (sha, ts) ->
      if ts > turn_ts then Some sha else None
    ) commits in
  (before, after)

(* Cache commits per-branch for the duration of index_session. *)
let make_branch_cache ~project_dir =
  let tbl = Hashtbl.create 4 in
  fun branch ->
    match Hashtbl.find_opt tbl branch with
    | Some cs -> cs
    | None ->
      let cs = load_branch_commits ~project_dir ~branch in
      Hashtbl.add tbl branch cs;
      cs

(* ---------- JSON helpers for row serialisation ---------- *)

let json_array_of_strings xs : string =
  Yojson.Safe.to_string (`List (List.map (fun s -> `String s) xs))

let nullable_text = function
  | "" -> S.Data.NULL
  | s -> S.Data.TEXT s

let int_data n = S.Data.INT (Int64.of_int n)

(* ---------- Session + step writes ---------- *)

let upsert_session db ~session_id ~started_at ~jsonl_path ~turn_count =
  D.exec_params db
    "INSERT INTO sessions(id, started_at, jsonl_path, turn_count, last_indexed_at) \
     VALUES(?, ?, ?, ?, ?) \
     ON CONFLICT(id) DO UPDATE SET \
       jsonl_path = excluded.jsonl_path, \
       turn_count = excluded.turn_count, \
       last_indexed_at = excluded.last_indexed_at"
    [ S.Data.TEXT session_id;
      S.Data.FLOAT started_at;
      S.Data.TEXT jsonl_path;
      S.Data.INT (Int64.of_int turn_count);
      S.Data.FLOAT (Unix.gettimeofday ()) ]

(* Upsert a step row keyed on (session_id, turn_index). Preserves the
   existing summary/tags on re-index so `urme init` can be run again
   without losing the Claude pass's work. *)
let upsert_step db
    ~session_id ~turn_index ~timestamp ~prompt_text ~files_touched
    ~commands_run ~thinking ~tokens_in ~tokens_out
    ~commit_before ~commit_after ~turn_json ~assistant_text =
  let thinking_json =
    if thinking = [] then S.Data.NULL
    else S.Data.TEXT (json_array_of_strings thinking) in
  D.exec_params db
    "INSERT INTO steps(\
       session_id, turn_index, timestamp, prompt_text, \
       files_touched, commands_run, thinking, tokens_in, tokens_out, \
       commit_before, commit_after, summary, tags, \
       turn_json, assistant_text) \
     VALUES(?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, NULL, NULL, ?, ?) \
     ON CONFLICT(session_id, turn_index) DO UPDATE SET \
       timestamp      = excluded.timestamp, \
       prompt_text    = excluded.prompt_text, \
       files_touched  = excluded.files_touched, \
       commands_run   = excluded.commands_run, \
       thinking       = excluded.thinking, \
       tokens_in      = excluded.tokens_in, \
       tokens_out     = excluded.tokens_out, \
       commit_before  = excluded.commit_before, \
       commit_after   = excluded.commit_after, \
       turn_json      = excluded.turn_json, \
       assistant_text = excluded.assistant_text"
    [ (match session_id with Some s -> S.Data.TEXT s | None -> S.Data.NULL);
      int_data turn_index;
      S.Data.FLOAT timestamp;
      nullable_text prompt_text;
      S.Data.TEXT (json_array_of_strings files_touched);
      S.Data.TEXT (json_array_of_strings commands_run);
      thinking_json;
      int_data tokens_in;
      int_data tokens_out;
      (match commit_before with Some s -> S.Data.TEXT s | None -> S.Data.NULL);
      (match commit_after  with Some s -> S.Data.TEXT s | None -> S.Data.NULL);
      nullable_text turn_json;
      nullable_text assistant_text ]

(* Delete any rows for [session_id] whose turn_index is no longer in
   [valid_set]. Used when a session JSONL has been edited and some old
   turns no longer exist (rare). *)
let prune_missing_turns db ~session_id ~valid_set =
  let sql =
    "SELECT turn_index FROM steps WHERE session_id = ?" in
  let existing =
    D.query_fold db sql [S.Data.TEXT session_id] ~init:[] ~f:(fun acc cols ->
      D.data_to_int cols.(0) :: acc)
  in
  List.iter (fun ti ->
    if not (List.mem ti valid_set) then
      D.exec_params db
        "DELETE FROM steps WHERE session_id = ? AND turn_index = ?"
        [S.Data.TEXT session_id; int_data ti]
  ) existing

(* ---------- Public entry point ---------- *)

let index_session ~db ~project_dir ~jsonl_path =
  let session_id = Jsonl.session_id_of_path jsonl_path in
  let interactions = Jsonl.parse_interactions ~filepath:jsonl_path in
  let started_at =
    match interactions with
    | i :: _ -> iso8601_to_epoch i.timestamp
    | [] -> Unix.gettimeofday ()
  in
  let branch_commits = make_branch_cache ~project_dir in
  D.with_txn db (fun () ->
    upsert_session db ~session_id ~started_at ~jsonl_path
      ~turn_count:(List.length interactions);
    let valid_turns = List.map (fun (i : interaction) -> i.index) interactions in
    prune_missing_turns db ~session_id ~valid_set:valid_turns;
    List.iter (fun (i : interaction) ->
      let turn_ts = iso8601_to_epoch i.timestamp in
      let jsons = jsons_in_range ~filepath:jsonl_path
          ~line_start:i.line_start ~line_end:i.line_end in
      let commands = extract_commands jsons in
      let thinking = extract_thinking jsons in
      let tokens_in, tokens_out = sum_tokens jsons in
      let commits = branch_commits i.branch in
      let commit_before, commit_after = stamp_bounds commits turn_ts in
      (* Persist the turn's full content (structured + flattened) so
         the TUI can still render it after Claude Code prunes the
         source JSONL, and so FTS matches the assistant's reasoning. *)
      let turn_json =
        raw_lines_in_range ~filepath:jsonl_path
          ~line_start:i.line_start ~line_end:i.line_end in
      let assistant_text = flatten_assistant_text jsons in
      upsert_step db
        ~session_id:(Some session_id)
        ~turn_index:i.index
        ~timestamp:turn_ts
        ~prompt_text:i.user_text
        ~files_touched:i.files_changed
        ~commands_run:commands
        ~thinking
        ~tokens_in ~tokens_out
        ~commit_before ~commit_after
        ~turn_json ~assistant_text
    ) interactions);
  List.length interactions

(* Detect urme's own summariser sessions so we don't index them. These are
   short sessions whose first user message starts with our summariser
   preamble. They get written to ~/.claude/projects/ when the daemon
   doesn't pass --no-session-persistence (old runs before the flag was
   added). *)
let is_summariser_session interactions =
  match interactions with
  | (i : interaction) :: _ ->
    let t = i.user_text in
    let starts_with p =
      String.length t >= String.length p
      && String.sub t 0 (String.length p) = p in
    starts_with "Input turns:" ||
    starts_with "You summarise turns from"
  | [] -> false

(* Skip files whose mtime ≤ sessions.last_indexed_at — untouched
   JSONLs don't need re-parsing. First run always processes every
   file since [last_indexed_at] is NULL. *)
let session_up_to_date db ~session_id ~jsonl_mtime =
  match
    D.query_fold db
      "SELECT last_indexed_at FROM sessions WHERE id = ?"
      [S.Data.TEXT session_id] ~init:None
      ~f:(fun _ cols -> D.data_to_float_opt cols.(0))
  with
  | Some last when last >= jsonl_mtime -> true
  | _ -> false

let index_all_sessions ~db ~project_dir =
  let jsonl_dir = Jsonl.find_jsonl_dir ~project_dir in
  let files = Jsonl.list_sessions ~jsonl_dir in
  List.fold_left (fun total path ->
    let mtime = try (Unix.stat path).Unix.st_mtime with _ -> 0.0 in
    let session_id = Jsonl.session_id_of_path path in
    if session_up_to_date db ~session_id ~jsonl_mtime:mtime then total
    else
      let interactions = Jsonl.parse_interactions ~filepath:path in
      if is_summariser_session interactions then total
      else total + index_session ~db ~project_dir ~jsonl_path:path
  ) 0 files

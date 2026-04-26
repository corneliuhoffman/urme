(* Smoke test for urme V2 indexer.

   - Creates a minimal synthetic JSONL session
   - Opens a fresh SQLite DB and runs the schema migrations
   - Invokes Indexer.index_session
   - Reads back from sessions/steps/steps_fts and asserts basic invariants

   Runs in a temp dir so it leaves no trace on disk. *)

module D = Urme_store.Db
module Schema = Urme_store.Schema
module S = Sqlite3

(* Build a JSONL-ish synthetic session: two user→assistant turns with
   representative tool_use blocks (Edit, Bash) and usage tokens. *)
let synthetic_jsonl = String.concat "\n" [
  {|{"type":"user","uuid":"u1","timestamp":"2026-04-01T10:00:00.000Z","gitBranch":"main","message":{"content":"write a hello world"}}|};
  {|{"type":"assistant","uuid":"a1","timestamp":"2026-04-01T10:00:05.000Z","message":{"content":[{"type":"text","text":"Sure"},{"type":"tool_use","name":"Edit","id":"t1","input":{"file_path":"hello.ml","old_string":"","new_string":"print_endline \"hello\""}},{"type":"tool_use","name":"Bash","id":"t2","input":{"command":"dune build"}}],"usage":{"input_tokens":120,"output_tokens":40}}}|};
  {|{"type":"user","uuid":"u2","timestamp":"2026-04-01T10:01:00.000Z","gitBranch":"main","message":{"content":"now add a test"}}|};
  {|{"type":"assistant","uuid":"a2","timestamp":"2026-04-01T10:01:10.000Z","message":{"content":[{"type":"text","text":"OK"},{"type":"tool_use","name":"Bash","id":"t3","input":{"command":"dune test"}}],"usage":{"input_tokens":80,"output_tokens":20}}}|};
  "";
]

let failf fmt = Printf.ksprintf failwith fmt

let assert_eq_int ~label expected actual =
  if expected <> actual then
    failf "ASSERT %s: expected %d, got %d" label expected actual

let query_one_int db sql =
  D.query_fold db sql [] ~init:0 ~f:(fun _ cols -> D.data_to_int cols.(0))

let () =
  let tmpdir = Filename.temp_file "urme-test-" "" in
  Sys.remove tmpdir;
  Unix.mkdir tmpdir 0o755;
  let jsonl_path = Filename.concat tmpdir "abc123.jsonl" in
  let oc = open_out jsonl_path in
  output_string oc synthetic_jsonl;
  close_out oc;

  let db_path = Filename.concat tmpdir "db.sqlite" in
  let db = Schema.open_at db_path in

  let n = Urme_engine.Indexer.index_session
      ~db ~project_dir:tmpdir ~jsonl_path in
  assert_eq_int ~label:"turn count returned" 2 n;

  (* sessions: one row *)
  let session_rows =
    query_one_int db "SELECT COUNT(*) FROM sessions" in
  assert_eq_int ~label:"sessions rows" 1 session_rows;

  (* steps: two rows *)
  let step_rows = query_one_int db "SELECT COUNT(*) FROM steps" in
  assert_eq_int ~label:"steps rows" 2 step_rows;

  (* files_touched: first turn should have hello.ml recorded *)
  let files_touched =
    D.query_fold db
      "SELECT files_touched FROM steps WHERE turn_index = 0"
      [] ~init:"" ~f:(fun _ cols -> D.data_to_string cols.(0)) in
  if not (String.length files_touched > 0
          && (try ignore (Str.search_forward (Str.regexp_string "hello.ml") files_touched 0); true with Not_found -> false))
  then failf "files_touched missing hello.ml: %s" files_touched;

  (* commands_run: turn 0 has 'dune build', turn 1 has 'dune test' *)
  let cmd0 = D.query_fold db
      "SELECT commands_run FROM steps WHERE turn_index = 0"
      [] ~init:"" ~f:(fun _ cols -> D.data_to_string cols.(0)) in
  if not (try ignore (Str.search_forward (Str.regexp_string "dune build") cmd0 0); true with Not_found -> false)
  then failf "commands_run turn0 missing 'dune build': %s" cmd0;

  (* tokens: turn 0 should have tokens_in=120, tokens_out=40 *)
  let tok_in =
    D.query_fold db "SELECT tokens_in FROM steps WHERE turn_index = 0"
      [] ~init:0 ~f:(fun _ cols -> D.data_to_int cols.(0)) in
  assert_eq_int ~label:"tokens_in turn0" 120 tok_in;

  (* Re-index is idempotent: row count stays the same *)
  let n2 = Urme_engine.Indexer.index_session
      ~db ~project_dir:tmpdir ~jsonl_path in
  assert_eq_int ~label:"reindex turn count" 2 n2;
  let step_rows_after = query_one_int db "SELECT COUNT(*) FROM steps" in
  assert_eq_int ~label:"steps rows after reindex" 2 step_rows_after;

  (* FTS5 trigger populates steps_fts on insert *)
  let fts_rows = query_one_int db "SELECT COUNT(*) FROM steps_fts" in
  assert_eq_int ~label:"steps_fts rows" 2 fts_rows;

  (* graph_version starts at 0 *)
  assert_eq_int ~label:"graph_version initial" 0 (Schema.graph_version db);

  Schema.close db;
  (* Cleanup *)
  (try Sys.remove db_path with _ -> ());
  (try Sys.remove jsonl_path with _ -> ());
  (try Unix.rmdir (Filename.concat tmpdir ".urme") with _ -> ());
  (try Unix.rmdir tmpdir with _ -> ());

  print_endline "test_indexer: OK"

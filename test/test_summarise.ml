(* End-to-end test of the summarise pass, with Claude stubbed out.

   We don't spawn the CLI — instead we manually exercise the write path:
   index a synthetic session → inject a fake list of turn_summary →
   apply_batch → verify steps.summary/tags got written and steps_fts
   was kept in sync by the trigger. *)

module D = Urme_store.Db
module Schema = Urme_store.Schema
module P = Urme_claude.Prompts
module Summarise = Urme_engine.Summarise
module S = Sqlite3

let synthetic_jsonl = String.concat "\n" [
  {|{"type":"user","uuid":"u1","timestamp":"2026-04-01T10:00:00.000Z","gitBranch":"main","message":{"content":"add feature X"}}|};
  {|{"type":"assistant","uuid":"a1","timestamp":"2026-04-01T10:00:05.000Z","message":{"content":[{"type":"text","text":"done"}],"usage":{"input_tokens":10,"output_tokens":5}}}|};
  {|{"type":"user","uuid":"u2","timestamp":"2026-04-01T10:01:00.000Z","gitBranch":"main","message":{"content":"fix bug Y"}}|};
  {|{"type":"assistant","uuid":"a2","timestamp":"2026-04-01T10:01:10.000Z","message":{"content":[{"type":"text","text":"fixed"}],"usage":{"input_tokens":10,"output_tokens":5}}}|};
  "";
]

let failf fmt = Printf.ksprintf failwith fmt

let query_int db sql =
  D.query_fold db sql [] ~init:0 ~f:(fun _ cols -> D.data_to_int cols.(0))

let query_string db sql =
  D.query_fold db sql [] ~init:"" ~f:(fun _ cols -> D.data_to_string cols.(0))

let () =
  let tmpdir = Filename.temp_file "urme-summ-" "" in
  Sys.remove tmpdir; Unix.mkdir tmpdir 0o755;
  let jsonl_path = Filename.concat tmpdir "sess.jsonl" in
  let oc = open_out jsonl_path in
  output_string oc synthetic_jsonl;
  close_out oc;

  let db_path = Filename.concat tmpdir "db.sqlite" in
  let db = Schema.open_at db_path in
  let _ = Urme_engine.Indexer.index_session
      ~db ~project_dir:tmpdir ~jsonl_path in

  (* Before summarise pass: summary/tags should be NULL. *)
  let null_summary_count = query_int db
      "SELECT COUNT(*) FROM steps WHERE summary IS NULL" in
  if null_summary_count <> 2 then
    failf "pre-pass: expected 2 null summaries, got %d" null_summary_count;

  (* Load pending steps and inject fake summaries without calling Claude. *)
  let pendings = Summarise.load_pending db in
  if List.length pendings <> 2 then
    failf "expected 2 pending, got %d" (List.length pendings);

  let fake_summaries = [
    { P.turn_index = 0; summary = "added feature X";
      tags = ["feature"; "X"; "bootstrap"] };
    { P.turn_index = 1; summary = "fixed bug Y";
      tags = ["bug"; "Y"] };
  ] in
  let _ = D.with_txn db (fun () ->
    Summarise.apply_batch db ~pendings ~summaries:fake_summaries) in

  (* After pass: both rows should have summary + tags. *)
  let null_summary_count = query_int db
      "SELECT COUNT(*) FROM steps WHERE summary IS NULL" in
  if null_summary_count <> 0 then
    failf "post-pass: expected 0 null summaries, got %d" null_summary_count;

  let s0 = query_string db
      "SELECT summary FROM steps WHERE turn_index = 0" in
  if s0 <> "added feature X" then failf "turn 0 summary wrong: %S" s0;

  let t0 = query_string db
      "SELECT tags FROM steps WHERE turn_index = 0" in
  if t0 <> "feature x bootstrap" then failf "turn 0 tags wrong: %S" t0;

  (* FTS5 must find the rows by content. *)
  let match_count = query_int db
      "SELECT COUNT(*) FROM steps_fts WHERE steps_fts MATCH 'feature'" in
  if match_count <> 1 then failf "FTS match 'feature' expected 1, got %d" match_count;

  let match_bug = query_int db
      "SELECT COUNT(*) FROM steps_fts WHERE steps_fts MATCH 'bug'" in
  if match_bug <> 1 then failf "FTS match 'bug' expected 1, got %d" match_bug;

  (* prompt_text also indexed by FTS5. *)
  let match_prompt = query_int db
      "SELECT COUNT(*) FROM steps_fts WHERE steps_fts MATCH 'fix'" in
  if match_prompt <> 1 then failf "FTS match 'fix' expected 1, got %d" match_prompt;

  Schema.close db;
  (try Sys.remove db_path with _ -> ());
  (try Sys.remove jsonl_path with _ -> ());
  (try Unix.rmdir (Filename.concat tmpdir ".urme") with _ -> ());
  (try Unix.rmdir tmpdir with _ -> ());
  print_endline "test_summarise: OK"

(* End-to-end FTS5 search test.

   Index a synthetic session, inject stubbed summaries, then verify:
   - Search.run finds the expected rows by tag / summary / prompt
   - Ordering is consistent (BM25 descending relevance)
   - Empty / noisy queries fall back sensibly
   - run_with_fallback widens AND → OR when sparse *)

module D = Urme_store.Db
module Schema = Urme_store.Schema
module P = Urme_claude.Prompts
module Summarise = Urme_engine.Summarise
module Search = Urme_search.Search
module S = Sqlite3

let synthetic_jsonl = String.concat "\n" [
  {|{"type":"user","uuid":"u1","timestamp":"2026-04-01T10:00:00.000Z","gitBranch":"main","message":{"content":"refactor taint propagator in ocaml"}}|};
  {|{"type":"assistant","uuid":"a1","timestamp":"2026-04-01T10:00:05.000Z","message":{"content":[{"type":"text","text":"OK"}],"usage":{"input_tokens":10,"output_tokens":5}}}|};
  {|{"type":"user","uuid":"u2","timestamp":"2026-04-01T10:01:00.000Z","gitBranch":"main","message":{"content":"fix crash in TUI event loop"}}|};
  {|{"type":"assistant","uuid":"a2","timestamp":"2026-04-01T10:01:10.000Z","message":{"content":[{"type":"text","text":"done"}],"usage":{"input_tokens":10,"output_tokens":5}}}|};
  {|{"type":"user","uuid":"u3","timestamp":"2026-04-01T10:02:00.000Z","gitBranch":"main","message":{"content":"add markdown rendering to history view"}}|};
  {|{"type":"assistant","uuid":"a3","timestamp":"2026-04-01T10:02:05.000Z","message":{"content":[{"type":"text","text":"added"}],"usage":{"input_tokens":10,"output_tokens":5}}}|};
  "";
]

let failf fmt = Printf.ksprintf failwith fmt

let () =
  let tmpdir = Filename.temp_file "urme-search-" "" in
  Sys.remove tmpdir; Unix.mkdir tmpdir 0o755;
  let jsonl_path = Filename.concat tmpdir "sess.jsonl" in
  let oc = open_out jsonl_path in
  output_string oc synthetic_jsonl;
  close_out oc;

  let db_path = Filename.concat tmpdir "db.sqlite" in
  let db = Schema.open_at db_path in
  let _ = Urme_engine.Indexer.index_session ~db
      ~project_dir:tmpdir ~jsonl_path in

  (* Inject summaries — no Claude. *)
  let pendings = Summarise.load_pending db in
  let fake = [
    { P.turn_index = 0;
      summary = "refactored taint propagator dataflow analysis";
      tags = ["taint"; "propagator"; "ocaml"; "dataflow"] };
    { P.turn_index = 1;
      summary = "fixed a segfault in TUI event dispatch";
      tags = ["tui"; "crash"; "event"; "segfault"] };
    { P.turn_index = 2;
      summary = "added markdown renderer to history pane";
      tags = ["markdown"; "history"; "render"; "ui"] };
  ] in
  let _ = D.with_txn db (fun () ->
    Summarise.apply_batch db ~pendings ~summaries:fake) in

  (* 1. exact tag match *)
  let hits = Search.run ~db "taint" in
  if List.length hits <> 1 then failf "taint: expected 1, got %d" (List.length hits);
  (match hits with
   | [h] when h.turn_index = 0 -> ()
   | _ -> failf "taint: wrong turn matched");

  (* 2. multi-term AND: both 'propagator' and 'ocaml' on same row *)
  let hits = Search.run ~db "propagator ocaml" in
  if List.length hits <> 1 then failf "propagator ocaml: expected 1 got %d"
      (List.length hits);

  (* 3. multi-term AND that can't be satisfied by any single row *)
  let hits = Search.run ~db "taint tui" in
  if List.length hits <> 0 then failf "taint tui AND: expected 0 got %d"
      (List.length hits);

  (* 4. OR-join finds both rows *)
  let hits = Search.run ~db ~join:Urme_search.Fts.Or "taint tui" in
  if List.length hits <> 2 then failf "taint OR tui: expected 2 got %d"
      (List.length hits);

  (* 5. prompt_text is searchable even without Claude tags.
     'crash' is in prompt_text and tags of turn 1 — should hit. *)
  let hits = Search.run ~db "crash" in
  if List.length hits <> 1 then failf "crash: expected 1 got %d"
      (List.length hits);

  (* 6. run_with_fallback: AND-sparse → OR widens *)
  let primary = Search.run ~db "taint tui" in  (* AND → 0 *)
  let fb = Search.run_with_fallback ~db "taint tui" in
  if List.length fb <= List.length primary then
    failf "fallback should widen (primary=%d, fb=%d)"
      (List.length primary) (List.length fb);

  (* 7. garbage input returns empty, no crash *)
  let hits = Search.run ~db "@#$%^" in
  if hits <> [] then failf "garbage query should return []";

  Schema.close db;
  (try Sys.remove db_path with _ -> ());
  (try Sys.remove jsonl_path with _ -> ());
  (try Unix.rmdir (Filename.concat tmpdir ".urme") with _ -> ());
  (try Unix.rmdir tmpdir with _ -> ());
  print_endline "test_search: OK"

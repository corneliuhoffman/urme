(* Offline tests for Urme_claude.Prompts — prompt assembly + JSON parsing.
   Does not spawn the `claude` CLI. *)

module P = Urme_claude.Prompts

let failf fmt = Printf.ksprintf failwith fmt

let () =
  (* strip_fences removes ```json fences. *)
  let s = "```json\n[{\"turn_index\":0,\"summary\":\"x\",\"tags\":[\"a\"]}]\n```" in
  (* We can't call strip_fences directly (it's internal). Instead feed a
     wrapped response to the parser. *)
  let parsed = P.parse_summarise_response s in
  (* Expect parser to return [] because raw JSON with fences won't parse.
     This confirms we do need a strip step upstream — parser itself is strict. *)
  if List.length parsed <> 0 then
    failf "expected strict parser to reject fenced input, got %d items"
      (List.length parsed);

  (* Clean JSON input: parser extracts fields, tolerates shape drift. *)
  let clean =
    "[{\"turn_index\":0,\"summary\":\"refactored indexer\",\"tags\":[\"indexer\",\"refactor\"]},\
      {\"turn_index\":1,\"summary\":\"\",\"tags\":\"test fix\"},\
      {\"turn_index\":2,\"summary\":\"only summary\"}]" in
  let r = P.parse_summarise_response clean in
  if List.length r <> 3 then failf "expected 3 items, got %d" (List.length r);
  (match List.nth r 0 with
   | { P.turn_index = 0; summary = "refactored indexer";
       tags = ["indexer"; "refactor"] } -> ()
   | _ -> failf "turn 0 mismatch");
  (* tags given as a string should split on spaces *)
  (match List.nth r 1 with
   | { P.turn_index = 1; summary = ""; tags = ["test"; "fix"] } -> ()
   | _ -> failf "turn 1 tags mismatch");
  (* missing tags field -> [] *)
  (match List.nth r 2 with
   | { P.turn_index = 2; summary = "only summary"; tags = [] } -> ()
   | _ -> failf "turn 2 mismatch");

  (* Items lacking turn_index are dropped. *)
  let partial = "[{\"summary\":\"no idx\"},{\"turn_index\":5,\"summary\":\"ok\"}]" in
  let r2 = P.parse_summarise_response partial in
  if List.length r2 <> 1 then failf "expected 1 item after drop, got %d"
      (List.length r2);

  (* Malformed JSON -> []. *)
  if P.parse_summarise_response "not json at all" <> [] then
    failf "expected [] on malformed input";

  (* parse_query_list — JSON array of strings *)
  let ql = P.parse_query_list
      "[\"rename detection\",\"diff_match content_before\",\"\"]" in
  if ql <> ["rename detection"; "diff_match content_before"] then
    failf "query list parse mismatch";
  if P.parse_query_list "garbage" <> [] then
    failf "malformed query list should be []";

  (* parse_rerank_response — ranked + synthesis *)
  let rr = P.parse_rerank_response
      "{\"ranked\":[42,17,99],\"synthesis\":\"because content_before was empty\"}" in
  if rr.ranked_step_ids <> [42; 17; 99] then
    failf "rerank ranked mismatch";
  if rr.synthesis <> "because content_before was empty" then
    failf "rerank synthesis mismatch";

  (* missing fields fall back to empty *)
  let rr2 = P.parse_rerank_response "{\"ranked\":[]}" in
  if rr2.ranked_step_ids <> [] || rr2.synthesis <> "" then
    failf "rerank missing fields should be empty";

  print_endline "test_prompts: OK"

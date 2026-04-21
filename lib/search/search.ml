(* FTS5-only search over the urme V2 store.

   The search pipeline here is intentionally dumb: tokenise → MATCH →
   ORDER BY bm25 → LIMIT. Graph traversal and Claude rerank live on top
   in later phases; this is the ground-truth layer they build on. *)

module D = Urme_store.Db
module S = Sqlite3

type hit = {
  step_id : int;
  session_id : string option;
  turn_index : int;
  timestamp : float;
  summary : string;
  tags : string;
  prompt_text : string;
  files_touched : string;   (* JSON array string *)
  commit_before : string option;
  commit_after : string option;
  score : float;            (* bm25 — lower = better *)
}

let default_limit = 20

let row_to_hit cols =
  { step_id = D.data_to_int cols.(0);
    session_id = D.data_to_string_opt cols.(1);
    turn_index = D.data_to_int cols.(2);
    timestamp = D.data_to_float cols.(3);
    summary = D.data_to_string cols.(4);
    tags = D.data_to_string cols.(5);
    prompt_text = D.data_to_string cols.(6);
    files_touched = D.data_to_string cols.(7);
    commit_before = D.data_to_string_opt cols.(8);
    commit_after = D.data_to_string_opt cols.(9);
    score = D.data_to_float cols.(10) }

let run ~db ?(limit=default_limit) ?(join=Fts.And) query =
  match Fts.build_match ~join query with
  | None -> []
  | Some match_expr ->
    let sql =
      "SELECT s.id, s.session_id, s.turn_index, s.timestamp, \
              COALESCE(s.summary, ''), COALESCE(s.tags, ''), \
              COALESCE(s.prompt_text, ''), COALESCE(s.files_touched, '[]'), \
              s.commit_before, s.commit_after, \
              bm25(steps_fts) AS score \
       FROM steps_fts \
       JOIN steps s ON s.id = steps_fts.rowid \
       WHERE steps_fts MATCH ? \
       ORDER BY score \
       LIMIT ?"
    in
    D.query_list db sql
      [S.Data.TEXT match_expr; S.Data.INT (Int64.of_int limit)]
      ~f:row_to_hit

(* Fallback-on-miss: run primary query; if fewer than [~min_hits], retry
   with OR-join; still sparse → return whatever we got. Callers that want
   Claude to rewrite the query can layer on top. *)
let run_with_fallback ~db ?(limit=default_limit) ?(min_hits=3) query =
  let primary = run ~db ~limit ~join:And query in
  if List.length primary >= min_hits then primary
  else
    let widened = run ~db ~limit ~join:Or query in
    if List.length widened >= List.length primary then widened
    else primary

(* ---------- Smart search (Claude rewrite + rerank) ---------- *)

(* Deduplicate by step_id, preserving the order of first occurrence. *)
let dedup_by_id hits =
  let seen = Hashtbl.create 16 in
  List.filter (fun (h : hit) ->
    if Hashtbl.mem seen h.step_id then false
    else (Hashtbl.add seen h.step_id (); true)
  ) hits

(* If the initial FTS5 pass is sparse (< [~min_hits_for_fast]), ask Claude
   for alternative phrasings and union their hits. Returns the merged hit
   list with duplicates dropped. *)
let expand_with_rewrite
    ~db ~binary ?(limit=default_limit) ?(min_hits_for_fast=3) query =
  let open Lwt.Syntax in
  let primary = run_with_fallback ~db ~limit query in
  if List.length primary >= min_hits_for_fast then Lwt.return primary
  else
    let weak_summaries =
      List.map (fun (h : hit) -> h.summary) primary
      |> List.filter (fun s -> s <> "")
      |> (fun xs -> if List.length xs > 5
           then List.filteri (fun i _ -> i < 5) xs
           else xs)
    in
    let* alts = Urme_claude.Prompts.rewrite_query ~binary
        ~original_query:query
        ~sparse_hit_summaries:weak_summaries in
    let extra = List.concat_map (fun q ->
      run ~db ~limit ~join:And q
    ) alts in
    Lwt.return (dedup_by_id (primary @ extra))

(* Bundle FTS5 expansion + Claude rerank + synthesis. Returns (ordered
   hits, synthesis). Ordering follows Claude's ranked step_ids; hits
   Claude didn't rank get appended at the end by original BM25.

   Prefers summarised rows for the rerank shortlist — an unsummarised
   row has no distilled content, so Claude can't judge it against
   summarised ones. Only falls back to unsummarised rows when the
   summarised set is too thin. *)
let run_smart
    ~db ~binary
    ?(limit=default_limit)
    ?(shortlist_size=24)
    query =
  let open Lwt.Syntax in
  let* candidates = expand_with_rewrite ~db ~binary
      ~limit:(max limit shortlist_size * 2) query in
  let with_summary, without_summary =
    List.partition (fun (h : hit) -> h.summary <> "") candidates in
  let shortlist =
    let primary =
      if List.length with_summary <= shortlist_size then with_summary
      else List.filteri (fun i _ -> i < shortlist_size) with_summary
    in
    if List.length primary >= shortlist_size then primary
    else
      (* Pad with unsummarised rows to reach shortlist_size. *)
      let need = shortlist_size - List.length primary in
      let pad = List.filteri (fun i _ -> i < need) without_summary in
      primary @ pad
  in
  let inputs = List.map (fun (h : hit) ->
    { Urme_claude.Prompts.step_id = h.step_id;
      turn_index = h.turn_index;
      summary = h.summary;
      tags = h.tags;
      prompt_excerpt =
        if String.length h.prompt_text > 160
        then String.sub h.prompt_text 0 160 ^ "..."
        else h.prompt_text;
      timestamp = h.timestamp }
  ) shortlist in
  let* { ranked_step_ids; synthesis } =
    Urme_claude.Prompts.rerank ~binary ~query ~candidates:inputs in
  let by_id = Hashtbl.create (List.length candidates) in
  List.iter (fun (h : hit) -> Hashtbl.add by_id h.step_id h) candidates;
  let ranked =
    List.filter_map (fun id -> Hashtbl.find_opt by_id id) ranked_step_ids in
  let ranked_set = List.fold_left (fun acc id -> id :: acc) []
      ranked_step_ids in
  let tail = List.filter (fun (h : hit) ->
    not (List.mem h.step_id ranked_set)) candidates in
  let final = ranked @ tail in
  let cut =
    if List.length final <= limit then final
    else List.filteri (fun i _ -> i < limit) final
  in
  Lwt.return (cut, synthesis)

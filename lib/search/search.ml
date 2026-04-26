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
    try
      D.query_list db sql
        [S.Data.TEXT match_expr; S.Data.INT (Int64.of_int limit)]
        ~f:row_to_hit
    with _ -> []

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

(* ---------- Deep search (NL→SQL + rerank + grounded answer) ----- *)

(* Execute a Prompts.query_spec as a SQL query. Tries progressively
   looser variants if the strictest one returns nothing:
     1) AND + require_summary + date bounds
     2) OR  + require_summary + date bounds
     3) OR  + no require_summary + date bounds
     4) OR  + no require_summary + no date bounds *)
let run_spec ~db (spec : Urme_claude.Prompts.query_spec) =
  let order = match spec.order_by with
    | Urme_claude.Prompts.Earliest -> "s.timestamp ASC, bm25(steps_fts)"
    | Latest -> "s.timestamp DESC, bm25(steps_fts)"
    | Relevance -> "bm25(steps_fts)"
  in
  let attempt ?(join=Fts.And) ?(require_summary=spec.require_summary)
              ?(use_dates=true) () =
    (* Fts.build_match sanitises tokens; if it yields None we have no
       usable FTS expression, so bail. *)
    match Fts.build_match ~join spec.fts_terms with
    | None -> []
    | Some fts_expr ->
      let where = ref [ "steps_fts MATCH ?" ] in
      let params = ref [ S.Data.TEXT fts_expr ] in
      if require_summary then
        where := "s.summary IS NOT NULL AND s.summary != ''" :: !where;
      if use_dates then begin
        (match spec.after with
         | Some ts ->
           where := "s.timestamp >= ?" :: !where;
           params := !params @ [ S.Data.FLOAT ts ]
         | None -> ());
        (match spec.before with
         | Some ts ->
           where := "s.timestamp <= ?" :: !where;
           params := !params @ [ S.Data.FLOAT ts ]
         | None -> ());
      end;
      let sql =
        Printf.sprintf
          "SELECT s.id, s.session_id, s.turn_index, s.timestamp, \
                  COALESCE(s.summary, ''), COALESCE(s.tags, ''), \
                  COALESCE(s.prompt_text, ''), COALESCE(s.files_touched, '[]'), \
                  s.commit_before, s.commit_after, \
                  COALESCE(bm25(steps_fts), 0.0) AS score \
           FROM steps_fts \
           JOIN steps s ON s.id = steps_fts.rowid \
           WHERE %s \
           ORDER BY %s \
           LIMIT %d"
          (String.concat " AND " !where) order spec.limit
      in
      (* FTS5 rejects some expressions at runtime (stray operators, bad
         quoting, etc.). Swallow the error and let the caller try a
         looser attempt rather than propagating a search-level failure. *)
      try D.query_list db sql !params ~f:row_to_hit
      with _ -> []
  in
  let r = attempt () in
  if r <> [] then r else
  let r = attempt ~join:Fts.Or () in
  if r <> [] then r else
  let r = attempt ~join:Fts.Or ~require_summary:false () in
  if r <> [] then r else
  attempt ~join:Fts.Or ~require_summary:false ~use_dates:false ()

(* Read the assistant text for a specific (session_id, turn_index) by
   parsing the JSONL. Returns "" if not found. *)
let fetch_assistant_text ~project_dir ~session_id ~turn_index =
  try
    let jsonl_dir =
      Jsonl_reader.find_jsonl_dir ~project_dir in
    let path = Filename.concat jsonl_dir (session_id ^ ".jsonl") in
    if not (Sys.file_exists path) then ""
    else
      let interactions =
        Jsonl_reader.parse_interactions ~filepath:path in
      match List.find_opt (fun (i : Urme_core.Types.interaction) ->
        i.index = turn_index) interactions with
      | Some i -> i.assistant_summary
      | None -> ""
  with _ -> ""

let rec take n = function
  | _ when n <= 0 -> []
  | [] -> []
  | x :: xs -> x :: take (n - 1) xs

(* The deep pipeline: 2 Claude calls.

   Layer 1 (NL → SQL spec): Claude parses query intent into a
   structured spec; we run it as a SQL query to get a preliminary
   candidate set with cascading fallback.

   Layer 2 (answer-with-ranking): Claude sees up to 10 top candidates,
   with FULL user+assistant text for the first 5. It returns BOTH the
   final ranked order AND a synthesis sentence — unified so the row
   cited in the synthesis is the one ranked first. *)
let run_deep ~db ~binary ?(limit=20) ?(project_dir=".") ?(only_ranked=false) query =
  let open Lwt.Syntax in
  let* spec = Urme_claude.Prompts.sql_rewrite ~binary ~query in
  let candidates = run_spec ~db spec in
  if candidates = [] then Lwt.return ([], "")
  else
    (* Top 10 candidates from BM25 ordering. Top 5 get full turn text. *)
    let short = take 10 candidates in
    let full_text_count = 5 in
    let answer_candidates =
      List.mapi (fun i (h : hit) ->
        let sid = Option.value h.session_id ~default:"" in
        let assistant =
          if i < full_text_count then
            fetch_assistant_text ~project_dir
              ~session_id:sid ~turn_index:h.turn_index
          else "" in
        let prompt =
          if i < full_text_count then h.prompt_text
          else if String.length h.prompt_text > 160
          then String.sub h.prompt_text 0 160 ^ "..."
          else h.prompt_text in
        { Urme_claude.Prompts.tc_step_id = h.step_id;
          tc_session_id = sid;
          tc_turn_index = h.turn_index;
          tc_timestamp = h.timestamp;
          tc_prompt_text = prompt;
          tc_assistant_text = assistant;
          tc_summary = h.summary }
      ) short in
    let* { ao_ranked_step_ids; ao_synthesis } =
      Urme_claude.Prompts.answer_from_text ~binary ~query
        ~candidates:answer_candidates in
    let by_id = Hashtbl.create (List.length candidates) in
    List.iter (fun (h : hit) -> Hashtbl.add by_id h.step_id h) candidates;
    let ranked =
      List.filter_map (fun id -> Hashtbl.find_opt by_id id) ao_ranked_step_ids
    in
    let tail = List.filter (fun (h : hit) ->
      not (List.mem h.step_id ao_ranked_step_ids)) candidates in
    let ordered = if only_ranked then ranked else ranked @ tail in
    let cut =
      if List.length ordered <= limit then ordered
      else take limit ordered in
    Lwt.return (cut, ao_synthesis)

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

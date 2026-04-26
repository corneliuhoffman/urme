(* Thin wrappers over Process.spawn_oneshot for the V2 indexing
   and search pipelines.

   Hard constraint: all Claude access goes through the `claude` CLI as a
   subprocess. No direct Anthropic API client. `binary` is the path to the
   CLI, usually `"claude"` resolved via PATH. *)

open Lwt.Syntax

(* ---------- Shared utilities ---------- *)

(* Drop markdown code fences ```json ... ``` if Claude wraps its output. *)
let strip_fences s =
  let s = String.trim s in
  let drop_prefix p s =
    let pl = String.length p and sl = String.length s in
    if sl >= pl && String.sub s 0 pl = p
    then String.sub s pl (sl - pl) |> String.trim
    else s
  in
  let drop_suffix p s =
    let pl = String.length p and sl = String.length s in
    if sl >= pl && String.sub s (sl - pl) pl = p
    then String.sub s 0 (sl - pl) |> String.trim
    else s
  in
  s
  |> drop_prefix "```json"
  |> drop_prefix "```"
  |> drop_suffix "```"
  |> String.trim

(* Run a one-shot prompt, return the final Result event's text.
   [model] defaults to None (= CLI's default model, typically Sonnet). *)
let ask ?model ~binary ~prompt () =
  let opts = { Process.default_opts with
               model;
               allowed_tools = [];
               max_turns = Some 1 } in
  let* proc = Process.spawn_oneshot
      ~cwd:"." ~opts ~binary ~prompt () in
  let buf = Buffer.create 1024 in
  let* () = Process.iter_events proc ~f:(fun event ->
    (match event with
     | Stream.Assistant_message { content; _ } ->
       Buffer.add_string buf (Stream.text_of_content content)
     | Stream.Result { result; is_error; _ } ->
       if is_error then
         Printf.eprintf "claude CLI error: %s\n%!" result
     | _ -> ());
    Lwt.return_unit) in
  let _ = Process.wait proc in
  Lwt.return (strip_fences (Buffer.contents buf))

(* ---------- Daemon pool for high-volume batching ---------- *)

(* A pool of K persistent claude daemons. Each daemon is used serially
   (one prompt at a time); the pool dispatches across them in parallel. *)

type pool = {
  daemons  : Process.daemon list;
  lwt_pool : Process.daemon Lwt_pool.t;
}

let spawn_pool ?model ?(size=3) ?system_prompt ~binary () =
  (* NOTE: --bare is disabled because it forces auth via ANTHROPIC_API_KEY
     or apiKeyHelper, never OAuth/keychain — which breaks the user's Max
     subscription. We keep --tools "" and the custom system-prompt,
     which are both fine under OAuth. *)
  let opts = { Process.default_opts with
               model;
               system_prompt;
               allowed_tools = [];
               bare = false;
               no_tools = true } in
  let* daemons = Lwt_list.map_s (fun _ ->
    Process.spawn_daemon ~cwd:"." ~opts ~binary ()
  ) (List.init size (fun _ -> ())) in
  (* Lwt_pool creates its resources lazily on first [use]. We pre-spawned
     all daemons already, so the "creator" just pops from the queue. *)
  let q = Queue.create () in
  List.iter (fun d -> Queue.push d q) daemons;
  let lwt_pool = Lwt_pool.create size (fun () ->
    Lwt.return (Queue.pop q)) in
  Lwt.return { daemons; lwt_pool }

let with_daemon pool f = Lwt_pool.use pool.lwt_pool f

let close_pool pool =
  Lwt_list.iter_s (fun d ->
    let* _ = Process.close_daemon d in Lwt.return_unit
  ) pool.daemons

(* Pool-aware ask: checks out a daemon, sends the prompt, releases. *)
let ask_via_pool pool ~prompt =
  with_daemon pool (fun d ->
    let* raw = Process.ask_daemon d ~prompt in
    Lwt.return (strip_fences raw))

(* ---------- Turn summarisation ---------- *)

type turn_input = {
  turn_index : int;
  user_text : string;
  assistant_text : string;
  files_touched : string list;
  commands_run : string list;
}

type turn_summary = {
  turn_index : int;
  summary : string;
  tags : string list;
}

(* Clamp long strings in prompt blocks so we don't blow out context. *)
let clamp n s =
  if String.length s <= n then s
  else String.sub s 0 n ^ "..."

let format_turn_block (t : turn_input) =
  let files = match t.files_touched with
    | [] -> "(none)"
    | xs -> String.concat ", " xs in
  let commands = match t.commands_run with
    | [] -> "(none)"
    | xs -> String.concat "\n    $ " ("" :: List.map (clamp 200) xs) in
  Printf.sprintf
    "--- turn %d ---\nUser: %s\n\nAssistant: %s\n\nFiles touched: %s\nCommands run:%s\n"
    t.turn_index
    (clamp 2000 t.user_text)
    (clamp 4000 t.assistant_text)
    files
    commands

(* The instructional preamble. Becomes --system-prompt so it's a
   cacheable constant across all batches, not part of each user prompt. *)
let summarise_system_prompt =
  String.concat "\n" [
    "You summarise turns from a Claude Code coding session so they can \
     be indexed in a full-text search database.";
    "";
    "For each input turn, produce:";
    "- `summary`: ONE compact sentence (<= 30 words) naming what was done \
     or asked. Keep concrete identifiers (file names, function names, \
     error messages) — those drive lexical search.";
    "- `tags`: 3 to 8 lowercase space-separated keywords. Prefer concrete \
     nouns and technical terms over generic words.";
    "";
    "CRITICAL: each input block starts with `--- turn N ---`. Copy N \
     VERBATIM into the `turn_index` field. Do NOT renumber. Turn numbers \
     are usually not sequential — they can be arbitrary integers like \
     17, 42, 152. Mismatched turn_index values cause the database write \
     to silently drop.";
    "";
    "Return a JSON array. ONE object per input turn, in the same order \
     they appear, with keys `turn_index` (integer — must match the \
     input), `summary` (string), `tags` (array of strings).";
    "Output ONLY the JSON array. No prose, no markdown fences.";
  ]

(* Per-batch user prompt. Keep it tight — just the turns. *)
let build_summarise_prompt (turns : turn_input list) =
  let turn_blocks = List.map format_turn_block turns in
  "Input turns:\n\n" ^ String.concat "\n" turn_blocks

(* Parse the model's JSON array into typed summaries. Tolerant of shape drift:
   missing `tags` becomes [], missing `summary` becomes "". Skips items that
   lack a usable `turn_index`. *)
let parse_summarise_response raw =
  match Yojson.Safe.from_string raw with
  | exception _ -> []
  | json ->
    let open Yojson.Safe.Util in
    let items = try to_list json with _ -> [] in
    List.filter_map (fun item ->
      match item |> member "turn_index" |> to_int_option with
      | None -> None
      | Some idx ->
        let summary = item |> member "summary" |> to_string_option
                      |> Option.value ~default:"" in
        let tags =
          match item |> member "tags" with
          | `List xs ->
            List.filter_map to_string_option xs
          | `String s ->
            String.split_on_char ' ' s
            |> List.filter (fun s -> String.trim s <> "")
          | _ -> []
        in
        Some { turn_index = idx; summary; tags }
    ) items

(* Haiku is plenty for the extraction-shaped summarise/tag task, and costs
   about a third of Sonnet. Rewrite + rerank below keep the default model
   because they benefit from stronger reasoning. *)
let summarise_model = Some "claude-haiku-4-5"

let summarise_batch ?(model=summarise_model) ~binary (turns : turn_input list) =
  match turns with
  | [] -> Lwt.return []
  | _ ->
    let prompt = build_summarise_prompt turns in
    let* raw = ask ?model ~binary ~prompt () in
    Lwt.return (parse_summarise_response raw)

(* Pool-backed summarise — reuse a daemon across batches. Callers that
   already have a pool (e.g. summarise_pending) should use this. *)
let summarise_batch_via_pool pool (turns : turn_input list) =
  match turns with
  | [] -> Lwt.return []
  | _ ->
    let prompt = build_summarise_prompt turns in
    let* raw = ask_via_pool pool ~prompt in
    Lwt.return (parse_summarise_response raw)

(* ---------- Query rewrite ---------- *)

(* Task: when FTS5 returns few or low-quality hits, ask Claude to propose
   alternative phrasings. This is the piece that fixes the "new files"
   miss we saw in comparison testing — the user types their mental model,
   Claude translates it into terms the indexer chose. *)

let build_rewrite_prompt ~original_query ~sparse_hit_summaries =
  let context =
    match sparse_hit_summaries with
    | [] -> "The current full-text index returned zero matches."
    | xs ->
      let xs' = List.mapi (fun i s -> Printf.sprintf "  %d. %s" (i + 1) s) xs in
      "The current full-text search returned these weak matches:\n"
      ^ String.concat "\n" xs'
  in
  String.concat "\n" [
    "You help rewrite search queries for an FTS5 index over a coding \
     session history database. Each row in the index is one turn of a \
     Claude Code session, with a short summary, space-separated tags, and \
     the raw user prompt.";
    "";
    "The user's query is: " ^ Printf.sprintf "%S" original_query;
    "";
    context;
    "";
    "Propose 2–4 alternative queries that are more likely to hit the intended \
     rows. Good alternatives:";
    "- replace verbs with concrete nouns from the likely summary vocabulary \
       (e.g. `freeze` → `deadlock`, `crash` → `segfault`)";
    "- add likely file or function identifiers that would appear in a summary";
    "- drop filler words (`why`, `how`, `did`, `was`) that FTS5 stopwords \
       don't help on";
    "";
    "Return ONLY a JSON array of strings, each one a candidate query. No prose.";
  ]

let parse_query_list raw =
  match Yojson.Safe.from_string raw with
  | exception _ -> []
  | `List xs ->
    List.filter_map (function
      | `String s when String.trim s <> "" -> Some (String.trim s)
      | _ -> None
    ) xs
  | _ -> []

let rewrite_query ~binary ~original_query ~sparse_hit_summaries =
  let prompt = build_rewrite_prompt ~original_query ~sparse_hit_summaries in
  let* raw = ask ~binary ~prompt () in
  Lwt.return (parse_query_list raw)

(* ---------- Layer 1: NL → structured SQL spec ---------- *)

type order_by = Earliest | Latest | Relevance

type query_spec = {
  fts_terms : string;
  order_by : order_by;
  limit : int;
  require_summary : bool;
  after : float option;
  before : float option;
}

let default_query_spec = {
  fts_terms = "";
  order_by = Relevance;
  limit = 50;
  require_summary = true;
  after = None;
  before = None;
}

let build_sql_rewrite_prompt query =
  String.concat "\n" [
    "You translate natural-language queries about a coding session \
     history into a structured retrieval spec.";
    "";
    "The DB is a table of turns. Each row has: prompt_text, summary, \
     tags, timestamp, session_id. An FTS5 index covers summary + tags \
     + prompt_text.";
    "";
    "User query: " ^ Printf.sprintf "%S" query;
    "";
    "Return a JSON object with keys:";
    "- `fts_terms`: space-separated keywords for a SQLite FTS5 MATCH \
     expression. Use concrete technical nouns, drop filler words.";
    "- `order_by`: \"earliest\" (for origin queries — \"when did we \
     first X\", \"when did X start\"), \"latest\" (\"what did we last \
     do about X\"), or \"relevance\" (default).";
    "- `limit`: integer, 30-100.";
    "- `require_summary`: bool. True for analytical queries, false for \
     meta/question-style.";
    "- `after` / `before`: ISO date (YYYY-MM-DD) or null. Temporal \
     bounds if the query says so.";
    "";
    "Examples:";
    "  \"when did we first look at sqlite?\" → \
     { fts_terms: \"sqlite\", order_by: \"earliest\", limit: 50, \
       require_summary: true, after: null, before: null }";
    "  \"what did we last do about git linking?\" → \
     { fts_terms: \"git link\", order_by: \"latest\", ... }";
    "  \"how did we fix the crash on 2026-04-15?\" → \
     { fts_terms: \"crash fix\", order_by: \"relevance\", \
       after: \"2026-04-15\", before: \"2026-04-16\" }";
    "";
    "Output ONLY the JSON. No prose.";
  ]

let parse_iso_date s =
  try
    Scanf.sscanf s "%4d-%2d-%2d" (fun y m d ->
      let dim = [|31;28;31;30;31;30;31;31;30;31;30;31|] in
      let leap yr = (yr mod 4 = 0 && yr mod 100 <> 0) || yr mod 400 = 0 in
      let rec yd acc yr = if yr <= 1970 then acc
        else yd (acc + if leap (yr - 1) then 366 else 365) (yr - 1) in
      let md = ref 0 in
      for mi = 0 to m - 2 do
        md := !md + dim.(mi);
        if mi = 1 && leap y then md := !md + 1
      done;
      Some (Float.of_int (((yd 0 y + !md + d - 1) * 86400))))
  with _ -> None

let parse_query_spec raw =
  match Yojson.Safe.from_string raw with
  | exception _ -> default_query_spec
  | json ->
    let open Yojson.Safe.Util in
    let s key default =
      try json |> member key |> to_string with _ -> default in
    let i key default =
      try json |> member key |> to_int with _ -> default in
    let b key default =
      try json |> member key |> to_bool with _ -> default in
    let date_opt key =
      match json |> member key with
      | `String d -> parse_iso_date d
      | _ -> None in
    let order_by = match String.lowercase_ascii (s "order_by" "relevance") with
      | "earliest" -> Earliest
      | "latest" -> Latest
      | _ -> Relevance in
    { fts_terms = s "fts_terms" "";
      order_by;
      limit = max 10 (min 200 (i "limit" 50));
      require_summary = b "require_summary" true;
      after = date_opt "after";
      before = date_opt "before" }

let sql_rewrite ~binary ~query =
  let prompt = build_sql_rewrite_prompt query in
  let* raw = ask ~binary ~prompt () in
  Lwt.return (parse_query_spec raw)

(* ---------- Layer 3: grounded answer from full turn text ---------- *)

type text_candidate = {
  tc_step_id : int;
  tc_session_id : string;
  tc_turn_index : int;
  tc_timestamp : float;
  tc_prompt_text : string;
  tc_assistant_text : string;
  tc_summary : string;
}

(* Answer output = final ranking (layer-3 aware of full text) plus the
   synthesis sentence. This supersedes the layer-2 rerank because it
   saw more evidence. *)
type answer_output = {
  ao_ranked_step_ids : int list;
  ao_synthesis : string;
}

let build_answer_prompt ~query ~(candidates : text_candidate list) =
  let blocks = List.map (fun c ->
    let tm = Unix.gmtime c.tc_timestamp in
    let date = Printf.sprintf "%04d-%02d-%02d"
      (tm.tm_year + 1900) (tm.tm_mon + 1) tm.tm_mday in
    let sid_short =
      if String.length c.tc_session_id >= 8
      then String.sub c.tc_session_id 0 8 else c.tc_session_id in
    Printf.sprintf
      "--- step_id=%d %s turn=%d date=%s ---\n\
       Summary: %s\n\
       User:\n%s\n\n\
       Assistant:\n%s\n"
      c.tc_step_id sid_short c.tc_turn_index date
      (clamp 200 c.tc_summary)
      (clamp 2000 c.tc_prompt_text)
      (clamp 3000 c.tc_assistant_text)
  ) candidates in
  String.concat "\n" [
    "You rank search results AND answer a question, using the \
     candidate turns below as evidence. Top candidates include the \
     full user+assistant text; later ones have summaries only.";
    "";
    "User query: " ^ Printf.sprintf "%S" query;
    "";
    "Candidates:";
    "";
    String.concat "\n" blocks;
    "";
    "Return a JSON object with:";
    "- `ranked`: array of step_id integers, best first. INCLUDE ONLY \
     candidates that genuinely support an answer to the query. EXCLUDE \
     candidates that are merely lexically related, tangentially \
     mentioned, or describe the OPPOSITE direction (e.g. if the user \
     asks when a feature was ADDED, exclude turns about REMOVING or \
     DROPPING it, and vice versa). When in doubt, leave the candidate \
     OUT. It is better to return 0 or 1 ids than to include unrelated \
     hits.";
    "- `synthesis`: ONE or TWO sentences answering the query, citing \
     the top-ranked candidate (session prefix + turn + date). Every \
     id you include in `ranked` MUST be cited or clearly support the \
     synthesis. Empty string if nothing answers.";
    "";
    "Output ONLY the JSON object. No prose, no fences.";
  ]

let parse_answer_response raw =
  match Yojson.Safe.from_string raw with
  | exception _ -> { ao_ranked_step_ids = []; ao_synthesis = "" }
  | json ->
    let open Yojson.Safe.Util in
    let ranked =
      match json |> member "ranked" with
      | `List xs ->
        List.filter_map (fun j -> try Some (to_int j) with _ -> None) xs
      | _ -> []
    in
    let synthesis =
      json |> member "synthesis" |> to_string_option
      |> Option.value ~default:""
    in
    { ao_ranked_step_ids = ranked; ao_synthesis = synthesis }

let answer_from_text ~binary ~query ~candidates =
  match candidates with
  | [] -> Lwt.return { ao_ranked_step_ids = []; ao_synthesis = "" }
  | _ ->
    let prompt = build_answer_prompt ~query ~candidates in
    let* raw = ask ~binary ~prompt () in
    Lwt.return (parse_answer_response raw)

(* ---------- Rerank ---------- *)

(* Take a shortlist of candidate rows (already scored by FTS5), hand them
   to Claude with the original query, and get back a reordering plus an
   optional one-paragraph synthesis. We use only step_id for mapping —
   Claude returns the step_ids in preferred order, no prose reshaping. *)

type rerank_input = {
  step_id : int;
  turn_index : int;
  summary : string;
  tags : string;
  prompt_excerpt : string;   (* first ~120 chars of prompt_text *)
  timestamp : float;         (* unix epoch, for chronology queries *)
}

type rerank_output = {
  ranked_step_ids : int list;
  synthesis : string;
}

let build_rerank_prompt ~query ~(candidates : rerank_input list) =
  let rows = List.map (fun c ->
    let tm = Unix.gmtime c.timestamp in
    let date = Printf.sprintf "%04d-%02d-%02d"
      (tm.tm_year + 1900) (tm.tm_mon + 1) tm.tm_mday in
    Printf.sprintf
      "- step_id=%d turn=%d date=%s\n  summary: %s\n  tags: %s\n  prompt: %s"
      c.step_id c.turn_index date
      (clamp 200 c.summary)
      (clamp 120 c.tags)
      (clamp 120 c.prompt_excerpt)
  ) candidates in
  String.concat "\n" [
    "You rerank search results for a coding-session history database.";
    "";
    "User query: " ^ Printf.sprintf "%S" query;
    "";
    "Candidate rows (unsorted):";
    String.concat "\n" rows;
    "";
    "Ranking rules:";
    "- PREFER rows whose SUMMARY describes a concrete action, decision, \
     or outcome that answers the query. The summary is the distilled \
     answer; the prompt is just what the user asked.";
    "- DEPRIORITISE rows that are just the user ASKING a similar \
     question (prompt repeats or paraphrases the query; summary is \
     empty or meta). These are meta-matches, not answers.";
    "- DEPRIORITISE rows with empty summary unless the prompt itself \
     uniquely answers the query.";
    "- CHRONOLOGY: if the query asks \"when did we first X\", \"when \
     did X start\", \"first time we discussed X\" etc., PREFER the \
     EARLIEST date among rows that genuinely touch X. Later \
     diagnoses/meta-discussions of X are not the origin.";
    "- DROP rows that clearly don't match.";
    "";
    "Return a JSON object with two keys:";
    "- `ranked`: array of step_id integers, best first, from the \
     candidates above.";
    "- `synthesis`: ONE short sentence answering the user's question, \
     grounded in the kept rows. Empty string if the candidates don't \
     answer it.";
    "";
    "Output ONLY the JSON object. No prose, no fences.";
  ]

let parse_rerank_response raw : rerank_output =
  match Yojson.Safe.from_string raw with
  | exception _ -> { ranked_step_ids = []; synthesis = "" }
  | json ->
    let open Yojson.Safe.Util in
    let ranked =
      match json |> member "ranked" with
      | `List xs ->
        List.filter_map (fun j -> try Some (to_int j) with _ -> None) xs
      | _ -> []
    in
    let synthesis =
      json |> member "synthesis" |> to_string_option
      |> Option.value ~default:""
    in
    { ranked_step_ids = ranked; synthesis }

let rerank ~binary ~query ~(candidates : rerank_input list) =
  match candidates with
  | [] -> Lwt.return { ranked_step_ids = []; synthesis = "" }
  | _ ->
    let prompt = build_rerank_prompt ~query ~candidates in
    let* raw = ask ~binary ~prompt () in
    Lwt.return (parse_rerank_response raw)

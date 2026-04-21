(* Fills in steps.summary / steps.tags by calling the Claude CLI in batches.

   One row per prompt→response turn goes to Claude; we batch ~8 turns per
   spawn so the prompt-cache amortises startup cost. FTS5 triggers
   auto-index the summary + tags the moment the UPDATE lands. *)

open Lwt.Syntax

module D = Urme_store.Db
module S = Sqlite3
module P = Urme_claude.Prompts
module Jsonl = Urme_search.Jsonl_reader

(* How many turns per prompt. Haiku handles larger batches fine, and
   per-daemon pipelining amortises claude-CLI startup anyway. *)
let default_batch_size = 16

(* How many persistent claude daemons to run in parallel. Each daemon
   pays the ~10s startup tax once; after that it processes batches
   back-to-back. *)
let default_pool_size = 3

(* ---------- Row loading ---------- *)

type pending_step = {
  step_id : int;
  session_id : string option;
  turn_index : int;
  prompt_text : string;
  jsonl_path : string option;  (* resolved from sessions.jsonl_path *)
  line_start : int option;
  line_end : int option;
  files_touched : string list;
  commands_run : string list;
}

let parse_json_array_of_strings s =
  match Yojson.Safe.from_string s with
  | `List xs ->
    List.filter_map (function `String s -> Some s | _ -> None) xs
  | _ -> []
  | exception _ -> []

(* Fetch all steps missing a summary, joined with their session's jsonl_path.
   Human-edit rows (session_id NULL) are skipped here — task #7 deferred. *)
let load_pending db =
  let sql =
    "SELECT s.id, s.session_id, s.turn_index, \
            COALESCE(s.prompt_text, ''), \
            ss.jsonl_path, \
            COALESCE(s.files_touched, '[]'), \
            COALESCE(s.commands_run, '[]') \
     FROM steps s \
     LEFT JOIN sessions ss ON ss.id = s.session_id \
     WHERE s.summary IS NULL AND s.session_id IS NOT NULL \
     ORDER BY s.session_id, s.turn_index"
  in
  D.query_list db sql [] ~f:(fun cols ->
    { step_id = D.data_to_int cols.(0);
      session_id = D.data_to_string_opt cols.(1);
      turn_index = D.data_to_int cols.(2);
      prompt_text = D.data_to_string cols.(3);
      jsonl_path = D.data_to_string_opt cols.(4);
      line_start = None;
      line_end = None;
      files_touched = parse_json_array_of_strings (D.data_to_string cols.(5));
      commands_run = parse_json_array_of_strings (D.data_to_string cols.(6)) })

(* Cache JSONL parse results. Previously we re-parsed the whole file
   for every turn — O(turns × filesize) per session. Now: O(filesize)
   per session. *)
let interactions_cache : (string, Urme_core.Types.interaction list) Hashtbl.t
  = Hashtbl.create 16

let cached_interactions ~jsonl_path =
  match Hashtbl.find_opt interactions_cache jsonl_path with
  | Some xs -> xs
  | None ->
    let xs = Jsonl.parse_interactions ~filepath:jsonl_path in
    Hashtbl.add interactions_cache jsonl_path xs;
    xs

let assistant_text_for ~jsonl_path ~turn_index =
  let interactions = cached_interactions ~jsonl_path in
  match List.find_opt (fun (i : Urme_core.Types.interaction) ->
    i.index = turn_index) interactions with
  | Some i -> P.clamp 4000 i.assistant_summary
  | None -> ""

(* ---------- Batching ---------- *)

(* Take first [n] of a list. *)
let rec take n = function
  | _ when n <= 0 -> []
  | [] -> []
  | x :: xs -> x :: take (n - 1) xs

let rec drop n = function
  | xs when n <= 0 -> xs
  | [] -> []
  | _ :: xs -> drop (n - 1) xs

let rec chunks n xs =
  match xs with
  | [] -> []
  | _ -> take n xs :: chunks n (drop n xs)

(* Build turn_input for a single pending step. *)
let turn_input_of ~pending =
  let assistant_text =
    match pending.jsonl_path with
    | Some p when Sys.file_exists p ->
      assistant_text_for ~jsonl_path:p ~turn_index:pending.turn_index
    | _ -> ""
  in
  { P.turn_index = pending.turn_index;
    user_text = pending.prompt_text;
    assistant_text;
    files_touched = pending.files_touched;
    commands_run = pending.commands_run }

(* ---------- Writeback ---------- *)

let space_join_tags tags =
  List.filter (fun s -> String.trim s <> "") tags
  |> List.map String.lowercase_ascii
  |> String.concat " "

let apply_count = ref 0

let apply_summary db ~step_id ~summary ~tags =
  incr apply_count;
  if !apply_count <= 3 || !apply_count mod 50 = 0 then
    Printf.eprintf "[apply #%d] step_id=%d summary_len=%d tags=%S\n%!"
      !apply_count step_id (String.length summary) (space_join_tags tags);
  D.exec_params db
    "UPDATE steps SET summary = ?, tags = ? WHERE id = ?"
    [ S.Data.TEXT summary;
      S.Data.TEXT (space_join_tags tags);
      S.Data.INT (Int64.of_int step_id) ]

(* Each batch: turn_index is unique within the batch (batches are built per
   session), so match by turn_index back to the originating step row. *)
(* Returns (applied, dropped) — dropped = summaries whose turn_index
   didn't match any pending row. Happens when Claude renumbers or drops
   entries. *)
let apply_batch db ~pendings ~summaries =
  let by_idx = Hashtbl.create (List.length pendings) in
  List.iter (fun (p : pending_step) -> Hashtbl.add by_idx p.turn_index p) pendings;
  let applied = ref 0 in
  let dropped = ref 0 in
  List.iter (fun (s : P.turn_summary) ->
    match Hashtbl.find_opt by_idx s.turn_index with
    | Some p ->
      apply_summary db ~step_id:p.step_id
        ~summary:s.summary ~tags:s.tags;
      incr applied
    | None -> incr dropped
  ) summaries;
  (!applied, !dropped)

(* ---------- Public entry points ---------- *)

(* Group pending rows by session, so each Claude batch stays within one
   session (turn_index is unique per session, which keeps the mapping
   simple). *)
let group_by_session pendings =
  let tbl = Hashtbl.create 16 in
  List.iter (fun (p : pending_step) ->
    let k = Option.value p.session_id ~default:"" in
    let xs = try Hashtbl.find tbl k with Not_found -> [] in
    Hashtbl.replace tbl k (p :: xs)
  ) pendings;
  Hashtbl.fold (fun _k xs acc -> List.rev xs :: acc) tbl []

let summarise_pending
    ?(batch_size=default_batch_size)
    ?(pool_size=default_pool_size)
    ~binary ~db () =
  let pendings = load_pending db in
  if pendings = [] then Lwt.return_unit
  else
    let groups = group_by_session pendings in
    let all_batches = List.concat_map (fun g -> chunks batch_size g) groups in
    let total = List.length all_batches in
    let done_ = ref 0 in
    Printf.printf "summarise: %d batches across %d sessions, %d parallel daemons\n%!"
      total (List.length groups) pool_size;
    let* pool = P.spawn_pool
        ?model:P.summarise_model
        ~system_prompt:P.summarise_system_prompt
        ~size:pool_size ~binary () in
    Lwt.finalize
      (fun () ->
         Lwt_list.iter_p (fun batch ->
           let inputs = List.map (fun p -> turn_input_of ~pending:p) batch in
           let* summaries = P.summarise_batch_via_pool pool inputs in
           let applied, dropped =
             D.with_txn db (fun () ->
               apply_batch db ~pendings:batch ~summaries) in
           incr done_;
           Printf.printf
             "  batch %d/%d done (%d applied, %d dropped, %d pending)\n%!"
             !done_ total applied dropped
             (List.length batch - applied);
           Lwt.return_unit
         ) all_batches)
      (fun () -> P.close_pool pool)

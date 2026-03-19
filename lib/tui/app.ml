open Lwt.Syntax

(* ---------- Lwt_mvar-based state cell ---------- *)
(* Synchronous peek: safe because Lwt is cooperative and the mvar
   always has a value (take resolves immediately, put resolves immediately
   on the now-empty mvar). No yield point between take and put. *)
let peek mvar =
  let p = Lwt_mvar.take mvar in
  match Lwt.poll p with
  | Some v -> Lwt.ignore_result (Lwt_mvar.put mvar v); Some v
  | None -> Lwt.cancel p; None

let update mvar f =
  let* v = Lwt_mvar.take mvar in
  Lwt_mvar.put mvar (f v)

let ncpu =
  try int_of_string (String.trim (Sys.getenv "URME_WORKERS"))
  with _ ->
    try
      let ic = Unix.open_process_in "sysctl -n hw.ncpu 2>/dev/null || nproc 2>/dev/null" in
      let n = try int_of_string (String.trim (input_line ic)) with _ -> 4 in
      ignore (Unix.close_process_in ic); max 2 n
    with _ -> 4

(* Worker pool: N workers pull from a shared queue — no barrier stalls.
   Lwt is cooperative so Queue access between yield points is safe. *)
let iter_workers ~n f items =
  let q = Queue.create () in
  List.iter (fun x -> Queue.push x q) items;
  let worker () =
    let rec loop () =
      match Queue.take_opt q with
      | None -> Lwt.return_unit
      | Some item -> let* () = f item in loop ()
    in loop ()
  in
  Lwt.join (List.init n (fun _ -> worker ()))

let iteri_workers ~n f items =
  let q = Queue.create () in
  List.iteri (fun i x -> Queue.push (i, x) q) items;
  let worker () =
    let rec loop () =
      match Queue.take_opt q with
      | None -> Lwt.return_unit
      | Some (i, item) -> let* () = f i item in loop ()
    in loop ()
  in
  Lwt.join (List.init n (fun _ -> worker ()))

(* Sort file paths by size descending — biggest first for better parallelism *)
let sort_by_size_desc paths =
  let with_size = List.filter_map (fun p ->
    try Some (p, (Unix.stat p).Unix.st_size)
    with _ -> Some (p, 0)) paths in
  List.sort (fun (_, a) (_, b) -> Int.compare b a) with_size
  |> List.map fst

(* ---------- Types ---------- *)

type ui_mode = Conv | Git | History

type pending_approval = {
  tool_name : string;
  tool_input : Yojson.Safe.t;
  resolve : bool -> unit;
}

type conv_entry =
  | User_msg of string
  | Assistant_text of string
  | Thinking_block of string
  | Tool_use_block of {
      tool_name : string;
      tool_use_id : string;
      input : Yojson.Safe.t;
      inline_diff : string option;
      timestamp : float;
    }
  | Tool_result_block of {
      tool_name : string;
      content : string;
      is_error : bool;
    }
  | System_info of string
  | Turn_separator

type git_focus = Branches | Commits | Files | Links

type git_conv_link = Urme_engine.Git_link_types.link

type git_state = {
  branches : string list;
  current_branch : string;
  commits : (string * float * string) list;
  files : string list;
  focus : git_focus;
  branch_idx : int;
  commit_idx : int;
  file_idx : int;
  link_idx : int;
  diff_preview : string;
  diff_scroll_git : int;
  file_diff_filter : string option;
  git_links : (string * string, git_conv_link list) Hashtbl.t;
    (* (commit_sha, file_basename) → links, multiple edits may map to same commit+file *)
  human_edits : (string * string, bool) Hashtbl.t;
    (* (commit_sha, file_basename) → true if human also edited *)
  link_candidates : git_conv_link list;
}

type history_state = {
  sessions : string list;           (* JSONL file paths, newest first *)
  session_idx : int;                (* which session file *)
  turns : conv_entry list list;     (* turns for current session *)
  turn_idx : int;                   (* which turn within session *)
  hist_scroll : int;
  return_mode : ui_mode;
  search_active : bool;
  search_query : string;
  search_results : (string * string * string * int * string * float) list;
    (* session_id, user_text, document, interaction_index, timestamp, distance *)
  result_idx : int;
  showing_results : bool;
}

let empty_git_state = {
  branches = []; current_branch = ""; commits = []; files = [];
  focus = Branches; branch_idx = 0; commit_idx = 0; file_idx = 0;
  link_idx = 0; diff_preview = ""; diff_scroll_git = 0;
  file_diff_filter = None; git_links = Hashtbl.create 0;
  human_edits = Hashtbl.create 0; link_candidates = [];
}

let empty_history_state = {
  sessions = []; session_idx = 0; turns = []; turn_idx = 0;
  hist_scroll = 0; return_mode = Conv;
  search_active = false; search_query = "";
  search_results = []; result_idx = 0; showing_results = false;
}

(* Split a flat conv_entry list into turns, each starting with a User_msg *)
let split_into_turns entries =
  let turns = ref [] in
  let current = ref [] in
  List.iter (fun e ->
    match e with
    | User_msg _ ->
      if !current <> [] then turns := List.rev !current :: !turns;
      current := [e]
    | Turn_separator -> ()  (* skip separators *)
    | _ -> current := e :: !current
  ) entries;
  if !current <> [] then turns := List.rev !current :: !turns;
  List.rev !turns

(* Filter out internal Claude CLI noise from text *)
let is_internal_noise s =
  let s = String.trim s in
  s = "" ||
  (let has_prefix p = String.length s >= String.length p &&
    String.sub s 0 (String.length p) = p in
   has_prefix "<local-command-caveat>" ||
   has_prefix "<system-reminder>" ||
   has_prefix "Unknown skill:" ||
   has_prefix "<" ||
   has_prefix "<command-" ||
   has_prefix "</")

(* Strip XML-like tags from text for display *)
let strip_xml_tags s =
  let buf = Buffer.create (String.length s) in
  let i = ref 0 in
  let len = String.length s in
  while !i < len do
    if s.[!i] = '<' then begin
      (* Skip until closing > *)
      let j = ref (!i + 1) in
      while !j < len && s.[!j] <> '>' do incr j done;
      if !j < len then i := !j + 1
      else (Buffer.add_char buf s.[!i]; incr i)
    end else begin
      Buffer.add_char buf s.[!i]; incr i
    end
  done;
  let result = Buffer.contents buf in
  let result = String.trim result in
  result


(* ---------- Command palette ---------- *)

type command_info = { cmd : string; description : string }

let builtin_commands = [
  { cmd = "/agents";   description = "List running background agents" };
  { cmd = "/clear";    description = "Clear conversation history" };
  { cmd = "/compact";  description = "Compact context with optional focus" };
  { cmd = "/context";  description = "Show context window usage" };
  { cmd = "/copy";     description = "Copy last response to clipboard" };
  { cmd = "/cost";     description = "Show token usage and cost" };
  { cmd = "/diff";     description = "Show accumulated diffs" };
  { cmd = "/exit";     description = "Exit urme" };
  { cmd = "/export";   description = "Export conversation as text" };
  { cmd = "/git";      description = "Open git browser" };
  { cmd = "/help";     description = "Show available commands" };
  { cmd = "/history";  description = "Browse past conversations (or /history <query> to search)" };
  { cmd = "/login";    description = "Sign in to Anthropic account" };
  { cmd = "/model";    description = "Show or switch model" };
  { cmd = "/plan";     description = "Enter plan mode" };
  { cmd = "/reset";    description = "Reset session (kill daemon)" };
  { cmd = "/skills";   description = "List available skills" };
  { cmd = "/status";   description = "Show session status" };
  { cmd = "/verbose";  description = "Toggle thinking blocks" };
]

(* Load custom commands from ~/.config/urme/commands.json
   Format: [{"cmd": "/foo", "description": "Do foo"}, ...] *)
let load_custom_commands () =
  let path = Filename.concat (Urme_core.Config.config_dir ()) "commands.json" in
  if Sys.file_exists path then
    try
      let ic = open_in path in
      let content = In_channel.input_all ic in
      close_in ic;
      let json = Yojson.Safe.from_string content in
      let open Yojson.Safe.Util in
      json |> to_list |> List.filter_map (fun entry ->
        try
          let cmd = entry |> member "cmd" |> to_string in
          let description = entry |> member "description" |> to_string in
          let cmd = if String.length cmd > 0 && cmd.[0] <> '/' then "/" ^ cmd else cmd in
          Some { cmd; description }
        with _ -> None)
    with _ -> []
  else []

let commands = lazy (
  let custom = load_custom_commands () in
  let builtin_names = List.map (fun c -> c.cmd) builtin_commands in
  let custom_new = List.filter (fun c ->
    not (List.mem c.cmd builtin_names)) custom in
  let all = builtin_commands @ custom_new in
  List.sort (fun a b -> String.compare a.cmd b.cmd) all
)

let filter_commands query =
  let q = String.lowercase_ascii query in
  List.filter (fun c ->
    let lc = String.lowercase_ascii c.cmd in
    let len = String.length q in
    if len = 0 then true
    else
      let rec has_sub i =
        if i + len > String.length lc then false
        else String.sub lc i len = q || has_sub (i + 1)
      in
      has_sub 0
  ) (Lazy.force commands)

(* Fully immutable state record *)
type state = {
  mode : ui_mode;
  entries : conv_entry list;
  input_text : string;
  scroll_offset : int;
  active_pane : [`Conv | `Diff];
  diff_text : string;
  diff_scroll : int;
  streaming : bool;
  stream_text : string;
  config : Urme_core.Config.config;
  project_dir : string;
  daemon : Urme_claude.Process.t option;
  pending : pending_approval option;
  perm_server : Permission_server.t option;
  ui : LTerm_ui.t option;
  status_model : string;
  context_tokens : int;  (* latest input_tokens = current context window fill *)
  status_tokens_in : int;
  status_tokens_out : int;
  status_cost : float;
  status_extra : string;
  tool_use_map : (string * string) list;
  verbose : bool;
  palette_open : bool;
  palette_selected : int;
  git : git_state;
  history : history_state;
  message_count : int;
  usage : Urme_core.Usage.usage_data option;
  started_chroma : bool;
  started_ollama : bool;
}

(* ---------- Helpers ---------- *)

let find_bridge_binary () =
  let make_abs p =
    if Filename.is_relative p then Filename.concat (Sys.getcwd ()) p else p in
  let candidates = [
    Filename.concat (Filename.dirname Sys.executable_name) "urme-permission-bridge";
    make_abs "_build/default/bin/bridge/permission_bridge.exe";
  ] in
  match List.find_opt Sys.file_exists candidates with
  | Some path -> make_abs path
  | None -> "urme-permission-bridge"

let initial_state ~config ~project_dir = {
  mode = Conv;
  entries = []; input_text = ""; scroll_offset = 0;
  active_pane = `Conv; diff_text = ""; diff_scroll = 0;
  streaming = false; stream_text = "";
  config; project_dir;
  daemon = None; pending = None; perm_server = None; ui = None;
  status_model = ""; context_tokens = 0; status_tokens_in = 0; status_tokens_out = 0;
  status_cost = 0.0; status_extra = "Ready";
  tool_use_map = []; verbose = false;
  palette_open = false; palette_selected = 0;
  git = empty_git_state; history = empty_history_state;
  message_count = 0; usage = None;
  started_chroma = false; started_ollama = false;
}

let append_diff s diff =
  if diff = "" then s
  else { s with
    diff_text = (if s.diff_text = "" then diff else s.diff_text ^ "\n" ^ diff);
    diff_scroll = 0;
  }

let redraw mvar =
  match peek mvar with
  | Some s -> (match s.ui with Some ui -> LTerm_ui.draw ui | None -> ())
  | None -> ()

(* ---------- Colors (from config theme) ---------- *)

let _theme = (Urme_core.Config.load ()).theme
let lc (c : Urme_core.Config.rgb) = LTerm_style.rgb c.r c.g c.b
let c_bg = lc _theme.bg
let c_fg = lc _theme.fg
let c_border = lc _theme.border
let c_user = lc _theme.user
let c_assistant = lc _theme.assistant
let c_status_bg = lc _theme.status_bg
let c_input_bg = lc _theme.input_bg
let c_highlight = lc _theme.highlight
let c_thinking = lc _theme.thinking
let c_tool_name = lc _theme.tool_name
let c_selection_bg = lc _theme.selection_bg
let c_tool_result = lc _theme.tool_result
let c_error = lc _theme.error
let c_diff_add_fg = lc _theme.diff_add_fg
let c_diff_add_bg = lc _theme.diff_add_bg
let c_diff_del_fg = lc _theme.diff_del_fg
let c_diff_del_bg = lc _theme.diff_del_bg
let c_separator = lc _theme.separator

(* ---------- UTF-8 / drawing ---------- *)

let sanitize_utf8 s =
  let buf = Buffer.create (String.length s) in
  let len = String.length s in
  let rec loop i =
    if i >= len then Buffer.contents buf
    else
      let byte = Char.code s.[i] in
      if byte < 0x80 then begin
        Buffer.add_char buf s.[i]; loop (i + 1)
      end else
        let expected =
          if byte land 0xE0 = 0xC0 then 2
          else if byte land 0xF0 = 0xE0 then 3
          else if byte land 0xF8 = 0xF0 then 4
          else 0 in
        if expected = 0 || i + expected > len then begin
          Buffer.add_char buf '?'; loop (i + 1)
        end else
          let rec valid j =
            j >= expected || (Char.code s.[i + j] land 0xC0 = 0x80 && valid (j + 1))
          in
          if valid 1 then begin
            Buffer.add_string buf (String.sub s i expected);
            loop (i + expected)
          end else begin
            Buffer.add_char buf '?'; loop (i + 1)
          end
  in
  loop 0

let draw_str ctx row col ~style text =
  let size = LTerm_draw.size ctx in
  if row >= 0 && row < size.rows then begin
    let max_len = max 0 (size.cols - col) in
    let text = if String.length text > max_len then
      String.sub text 0 max_len else text in
    if String.length text > 0 then
      LTerm_draw.draw_string ctx row col
        ~style (Zed_string.of_utf8 (sanitize_utf8 text))
  end

let wrap_text text width =
  if width <= 0 then [""]
  else
    String.split_on_char '\n' text
    |> List.concat_map (fun line ->
      if String.length line <= width then [line]
      else
        let rec split acc pos =
          if pos >= String.length line then List.rev acc
          else
            let len = min width (String.length line - pos) in
            split (String.sub line pos len :: acc) (pos + len)
        in
        split [] 0)

(* ---------- Tool helpers ---------- *)

let summarize_tool_input tool_name input =
  let open Yojson.Safe.Util in
  match tool_name with
  | "Bash" ->
    let cmd = (try input |> member "command" |> to_string with _ -> "?") in
    if String.length cmd > 80 then String.sub cmd 0 77 ^ "..." else cmd
  | "Edit" | "Write" | "Read" ->
    (try input |> member "file_path" |> to_string with _ -> "?")
  | _ -> ""

(* Read a file into lines, returning [] on error *)
let read_file_lines path =
  try
    let ic = open_in path in
    let rec loop acc =
      match input_line ic with
      | line -> loop (line :: acc)
      | exception End_of_file -> close_in ic; List.rev acc
    in
    loop []
  with _ -> []

(* Find the 0-based line index where needle starts in lines, or None *)
let find_substring_start lines needle_lines =
  let nl = List.length needle_lines in
  if nl = 0 then Some 0
  else
    let arr = Array.of_list lines in
    let narr = Array.of_list needle_lines in
    let len = Array.length arr in
    let rec search i =
      if i + nl > len then None
      else
        let rec matches j =
          if j >= nl then true
          else arr.(i + j) = narr.(j) && matches (j + 1)
        in
        if matches 0 then Some i else search (i + 1)
    in
    search 0

(* Produce a unified diff of the full file for an Edit operation *)
let unified_diff_edit path old_s new_s =
  let orig_lines = read_file_lines path in
  let old_lines = String.split_on_char '\n' old_s in
  let new_lines = String.split_on_char '\n' new_s in
  let ctx = 3 in
  let buf = Buffer.create 1024 in
  Buffer.add_string buf (Printf.sprintf "--- %s\n+++ %s\n" path path);
  match find_substring_start orig_lines old_lines with
  | None ->
    (* Fallback: just show old/new without file context *)
    List.iter (fun l -> Buffer.add_string buf ("- " ^ l ^ "\n")) old_lines;
    List.iter (fun l -> Buffer.add_string buf ("+ " ^ l ^ "\n")) new_lines;
    Buffer.contents buf
  | Some start_idx ->
    let old_len = List.length old_lines in
    let new_len = List.length new_lines in
    let total = List.length orig_lines in
    let ctx_start = max 0 (start_idx - ctx) in
    let ctx_end = min total (start_idx + old_len + ctx) in
    let old_hunk_len = ctx_end - ctx_start in
    let new_hunk_len = old_hunk_len - old_len + new_len in
    Buffer.add_string buf
      (Printf.sprintf "@@ -%d,%d +%d,%d @@\n"
         (ctx_start + 1) old_hunk_len (ctx_start + 1) new_hunk_len);
    (* Context before *)
    for i = ctx_start to start_idx - 1 do
      Buffer.add_string buf (" " ^ List.nth orig_lines i ^ "\n")
    done;
    (* Removed lines *)
    List.iter (fun l -> Buffer.add_string buf ("- " ^ l ^ "\n")) old_lines;
    (* Added lines *)
    List.iter (fun l -> Buffer.add_string buf ("+ " ^ l ^ "\n")) new_lines;
    (* Context after *)
    for i = start_idx + old_len to ctx_end - 1 do
      Buffer.add_string buf (" " ^ List.nth orig_lines i ^ "\n")
    done;
    Buffer.contents buf

let diff_of_tool_input tool_name input =
  let open Yojson.Safe.Util in
  match tool_name with
  | "Edit" ->
    let path = (try input |> member "file_path" |> to_string with _ -> "?") in
    let old_s = (try input |> member "old_string" |> to_string with _ -> "") in
    let new_s = (try input |> member "new_string" |> to_string with _ -> "") in
    unified_diff_edit path old_s new_s
  | "Write" ->
    let path = (try input |> member "file_path" |> to_string with _ -> "?") in
    let content = (try input |> member "content" |> to_string with _ -> "") in
    let buf = Buffer.create 256 in
    Buffer.add_string buf (Printf.sprintf "+++ %s (new file)\n" path);
    List.iter (fun l -> Buffer.add_string buf ("+ " ^ l ^ "\n"))
      (String.split_on_char '\n' content);
    Buffer.contents buf
  | "Bash" ->
    Printf.sprintf "$ %s\n"
      (try Yojson.Safe.Util.(input |> member "command" |> to_string) with _ -> "?")
  | _ -> ""

(* Parse a session JSONL file into conv_entry list *)
let load_session_entries filepath =
  let entries = ref [] in
  let ic = open_in filepath in
  (try while true do
    let line = input_line ic in
    (try
      let json = Yojson.Safe.from_string line in
      let open Yojson.Safe.Util in
      let typ = json |> member "type" |> to_string_option
                |> Option.value ~default:"" in
      (match typ with
       | "user" ->
         let content = json |> member "message" |> member "content" in
         (match content with
          | `String s ->
            if not (is_internal_noise s) then
              entries := User_msg (strip_xml_tags s) :: !entries
          | `List items ->
            List.iter (fun item ->
              match item |> member "type" |> to_string_option with
              | Some "tool_result" ->
                let tid = item |> member "tool_use_id" |> to_string_option
                          |> Option.value ~default:"" in
                let c = (try item |> member "content" |> to_string
                         with _ -> "(result)") in
                let is_err = (try item |> member "is_error" |> to_bool
                              with _ -> false) in
                if not (is_internal_noise c) then
                  entries := Tool_result_block {
                    tool_name = tid; content = c; is_error = is_err } :: !entries
              | Some "text" ->
                let t = item |> member "text" |> to_string_option
                        |> Option.value ~default:"" in
                if not (is_internal_noise t) then
                  entries := User_msg (strip_xml_tags t) :: !entries
              | _ -> ()
            ) items
          | _ -> ())
       | "assistant" ->
         let ts_str = json |> member "timestamp" |> to_string_option
                      |> Option.value ~default:"" in
         let ts = Urme_core.Types.iso8601_to_epoch ts_str in
         let blocks = json |> member "message" |> member "content"
           |> (fun j -> try to_list j with _ -> []) in
         List.iter (fun block ->
           match block |> member "type" |> to_string_option with
           | Some "text" ->
             let t = block |> member "text" |> to_string_option
                     |> Option.value ~default:"" in
             if t <> "" then entries := Assistant_text t :: !entries
           | Some "thinking" ->
             let t = block |> member "thinking" |> to_string_option
                     |> Option.value ~default:"" in
             if t <> "" then entries := Thinking_block t :: !entries
           | Some "tool_use" ->
             let name = block |> member "name" |> to_string_option
                        |> Option.value ~default:"tool" in
             let tuid = block |> member "id" |> to_string_option
                        |> Option.value ~default:"" in
             let input = (try block |> member "input" with _ -> `Null) in
             let inline_diff = match name with
               | "Edit" | "Write" | "Bash" -> Some (diff_of_tool_input name input)
               | _ -> None in
             entries := Tool_use_block { tool_name = name; tool_use_id = tuid;
                                         input; inline_diff; timestamp = ts } :: !entries
           | _ -> ()
         ) blocks
       | "result" ->
         entries := Turn_separator :: !entries
       | _ -> ())
     with _ -> ())
  done with End_of_file -> ());
  close_in ic;
  List.rev !entries

(* Split session into interaction-aligned turns by parsing JSONL directly.
   Each turn = one real user interaction (type=user, content=String → assistant response).
   Esc/cancel and tool result messages don't start new turns. *)
let split_into_interaction_turns ~filepath =
  let turns = ref [] in
  let current = ref [] in
  let flush_turn () =
    if !current <> [] then turns := List.rev !current :: !turns;
    current := [] in
  let ic = open_in filepath in
  (try while true do
    let line = input_line ic in
    (try
      let json = Yojson.Safe.from_string line in
      let open Yojson.Safe.Util in
      let typ = json |> member "type" |> to_string_option
                |> Option.value ~default:"" in
      (match typ with
       | "user" ->
         let content = json |> member "message" |> member "content" in
         (match content with
          | `String s ->
            (* Real user message — start new turn *)
            if not (is_internal_noise s) then begin
              flush_turn ();
              current := [User_msg (strip_xml_tags s)]
            end
          | `List items ->
            (* Tool result / continuation — accumulate into current turn *)
            List.iter (fun item ->
              match item |> member "type" |> to_string_option with
              | Some "tool_result" ->
                let tid = item |> member "tool_use_id" |> to_string_option
                          |> Option.value ~default:"" in
                let c = (try item |> member "content" |> to_string
                         with _ -> "(result)") in
                let is_err = (try item |> member "is_error" |> to_bool
                              with _ -> false) in
                if not (is_internal_noise c) then
                  current := Tool_result_block {
                    tool_name = tid; content = c; is_error = is_err } :: !current
              | Some "text" ->
                let t = item |> member "text" |> to_string_option
                        |> Option.value ~default:"" in
                if not (is_internal_noise t) then
                  current := System_info (strip_xml_tags t) :: !current
              | _ -> ()
            ) items
          | _ -> ())
       | "assistant" ->
         let ts_str = json |> member "timestamp" |> to_string_option
                      |> Option.value ~default:"" in
         let ts = Urme_core.Types.iso8601_to_epoch ts_str in
         let blocks = json |> member "message" |> member "content"
           |> (fun j -> try to_list j with _ -> []) in
         List.iter (fun block ->
           match block |> member "type" |> to_string_option with
           | Some "text" ->
             let t = block |> member "text" |> to_string_option
                     |> Option.value ~default:"" in
             if t <> "" then current := Assistant_text t :: !current
           | Some "thinking" ->
             let t = block |> member "thinking" |> to_string_option
                     |> Option.value ~default:"" in
             if t <> "" then current := Thinking_block t :: !current
           | Some "tool_use" ->
             let name = block |> member "name" |> to_string_option
                        |> Option.value ~default:"tool" in
             let tuid = block |> member "id" |> to_string_option
                        |> Option.value ~default:"" in
             let input = (try block |> member "input" with _ -> `Null) in
             let inline_diff = match name with
               | "Edit" | "Write" | "Bash" -> Some (diff_of_tool_input name input)
               | _ -> None in
             current := Tool_use_block { tool_name = name; tool_use_id = tuid;
                                         input; inline_diff; timestamp = ts } :: !current
           | _ -> ()
         ) blocks
       | _ -> ())
    with _ -> ())
  done with End_of_file -> ());
  flush_turn ();
  close_in ic;
  List.rev !turns

let truncate_result content =
  let lines = String.split_on_char '\n' content in
  let total = List.length lines in
  if total <= 20 && String.length content <= 1000 then content
  else
    let kept = List.filteri (fun i _ -> i < 20) lines in
    let text = String.concat "\n" kept in
    let text = if String.length text > 1000
      then String.sub text 0 1000 ^ "..." else text in
    if total > 20 then
      text ^ Printf.sprintf "\n... (%d lines omitted)" (total - 20)
    else text

(* ---------- Rendering ---------- *)

type styled_line = {
  fg : LTerm_style.color;
  bg : LTerm_style.color option;
  text : string;
}

let render_entry ~width ~verbose entry =
  let mk ?(bg=None) fg text = { fg; bg; text } in
  let wrap_prefix prefix fg lines =
    List.mapi (fun i line ->
      mk fg (if i = 0 then prefix ^ line
             else String.make (String.length prefix) ' ' ^ line)
    ) lines
  in
  match entry with
  | User_msg text ->
    wrap_prefix "> " c_user (wrap_text text (width - 2)) @ [mk c_fg ""]
  | Assistant_text text ->
    wrap_prefix "  " c_assistant (wrap_text text (width - 2))
  | Thinking_block text ->
    if verbose then wrap_prefix "  [thinking] " c_thinking (wrap_text text (width - 14))
    else []
  | Tool_use_block { tool_name; input; inline_diff; _ } ->
    let header = Printf.sprintf "  [%s] %s" tool_name
        (summarize_tool_input tool_name input) in
    let hdr = List.map (mk c_tool_name) (wrap_text header width) in
    let diff = match inline_diff with
      | None -> []
      | Some diff ->
        let lines = String.split_on_char '\n' diff in
        let lines = if List.length lines > 30 then
          List.filteri (fun i _ -> i < 30) lines @
          [Printf.sprintf "... (%d more lines)" (List.length lines - 30)]
        else lines in
        let parse_hunk line =
          (* Parse @@ -old_start,old_len +new_start,new_len @@ *)
          try Scanf.sscanf line "@@ -%d,%d +%d,%d @@%_s"
                (fun o _ n _ -> Some (o, n))
          with _ ->
            try Scanf.sscanf line "@@ -%d +%d,%d @@%_s"
                  (fun o n _ -> Some (o, n))
            with _ ->
              try Scanf.sscanf line "@@ -%d,%d +%d @@%_s"
                    (fun o _ n -> Some (o, n))
              with _ -> None
        in
        let old_ln = ref 0 in
        let new_ln = ref 0 in
        List.map (fun line ->
          if String.length line > 0 then match line.[0] with
            | '@' ->
              (match parse_hunk line with
               | Some (o, n) -> old_ln := o; new_ln := n
               | None -> ());
              mk c_tool_name ("    " ^ line)
            | '+' ->
              let prefix = Printf.sprintf "    %4d " !new_ln in
              new_ln := !new_ln + 1;
              mk ~bg:(Some c_diff_add_bg) c_diff_add_fg (prefix ^ line)
            | '-' ->
              let prefix = Printf.sprintf " %4d    " !old_ln in
              old_ln := !old_ln + 1;
              mk ~bg:(Some c_diff_del_bg) c_diff_del_fg (prefix ^ line)
            | _ ->
              let prefix = Printf.sprintf " %4d %4d " !old_ln !new_ln in
              old_ln := !old_ln + 1; new_ln := !new_ln + 1;
              mk c_fg (prefix ^ line)
          else mk c_fg ""
        ) lines
    in
    hdr @ diff
  | Tool_result_block { content; is_error; _ } ->
    let color = if is_error then c_error else c_tool_result in
    List.map (fun l -> mk color ("    " ^ l))
      (wrap_text (truncate_result content) (width - 4)) @ [mk c_fg ""]
  | System_info text ->
    wrap_prefix "  " c_error (wrap_text text (width - 2))
  | Turn_separator ->
    [mk c_separator ("  " ^ String.make (min 40 (width - 2)) '-'); mk c_fg ""]

let draw_conversation ctx state =
  let size = LTerm_draw.size ctx in
  LTerm_draw.fill_style ctx
    LTerm_style.{ none with background = Some c_bg; foreground = Some c_fg };
  let title = if state.verbose then " Conversation [verbose]" else " Conversation" in
  draw_str ctx 0 0
    ~style:LTerm_style.{ none with background = Some c_status_bg;
                                    foreground = Some c_highlight; bold = Some true }
    (Printf.sprintf "%s%s" title (String.make (max 0 (size.cols - String.length title)) ' '));
  let width = max 1 (size.cols - 2) in
  let all_lines =
    List.concat_map (render_entry ~width ~verbose:state.verbose)
      (List.rev state.entries) in
  let all_lines =
    if state.streaming && state.stream_text <> "" then
      all_lines @ List.map (fun l -> { fg = c_assistant; bg = None; text = "  " ^ l })
        (wrap_text state.stream_text width)
    else all_lines in
  let visible_height = max 0 (size.rows - 1) in
  let total = List.length all_lines in
  let start = max 0 (total - visible_height - state.scroll_offset) in
  List.filteri (fun i _ -> i >= start && i < start + visible_height) all_lines
  |> List.iteri (fun i line ->
    let row = 1 + i in
    if row < size.rows then
      draw_str ctx row 1
        ~style:LTerm_style.{ none with
          background = Some (match line.bg with Some b -> b | None -> c_bg);
          foreground = Some line.fg }
        line.text)

let draw_diff ctx state =
  let size = LTerm_draw.size ctx in
  LTerm_draw.fill_style ctx
    LTerm_style.{ none with background = Some c_bg; foreground = Some c_fg };
  draw_str ctx 0 0
    ~style:LTerm_style.{ none with background = Some c_status_bg;
                                    foreground = Some c_highlight; bold = Some true }
    (Printf.sprintf " Diff %s" (String.make (max 0 (size.cols - 6)) ' '));
  if state.diff_text = "" then
    draw_str ctx 2 2
      ~style:LTerm_style.{ none with background = Some c_bg; foreground = Some c_border }
      "(no diff)"
  else begin
    let lines = String.split_on_char '\n' state.diff_text in
    let vh = max 0 (size.rows - 1) in
    let total = List.length lines in
    let start = max 0 (total - vh - state.diff_scroll) in
    List.filteri (fun i _ -> i >= start && i < start + vh) lines
    |> List.iteri (fun i line ->
      let row = 1 + i in
      if row < size.rows then
        let fg, bg = if String.length line > 0 then match line.[0] with
          | '+' -> (c_diff_add_fg, c_diff_add_bg)
          | '-' -> (c_diff_del_fg, c_diff_del_bg)
          | '@' -> (c_tool_name, c_bg)
          | _ -> (c_fg, c_bg)
        else (c_fg, c_bg) in
        draw_str ctx row 1
          ~style:LTerm_style.{ none with background = Some bg; foreground = Some fg }
          line)
  end

(* ---------- Git mode drawing ---------- *)

let draw_git_panel ctx ~row_start ~height ~title ~focused ~items ~sel_idx ~width =
  if height <= 0 then 0
  else begin
    let hdr_bg = if focused then c_highlight else c_status_bg in
    let hdr_fg = if focused then c_bg else c_highlight in
    draw_str ctx row_start 0
      ~style:LTerm_style.{ none with background = Some hdr_bg;
                                      foreground = Some hdr_fg; bold = Some true }
      (Printf.sprintf " %s %s" title (String.make (max 0 (width - String.length title - 2)) ' '));
    let content_h = max 0 (height - 1) in
    let _n = List.length items in
    let scroll = max 0 (sel_idx - content_h + 1) in
    List.iteri (fun i (label, is_current, is_dim) ->
      let vi = i - scroll in
      if vi >= 0 && vi < content_h then begin
        let row = row_start + 1 + vi in
        let selected = focused && i = sel_idx in
        let fg = if selected then c_highlight
                 else if is_dim then c_separator  (* no data *)
                 else if is_current then c_user    (* human edited *)
                 else c_fg in                      (* claude only *)
        let bg = if selected then c_status_bg else c_bg in
        let label_trunc = if String.length label > width - 1
          then String.sub label 0 (width - 1) else label in
        draw_str ctx row 0
          ~style:LTerm_style.{ none with foreground = Some fg; background = Some bg;
                                          bold = Some selected }
          label_trunc
      end
    ) items;
    height
  end

let commit_has_links g sha =
  Hashtbl.fold (fun (s, _) _ found -> found || s = sha) g.git_links false

let next_linked_commit g from =
  let n = List.length g.commits in
  let rec find i =
    if i >= n then from
    else match List.nth_opt g.commits i with
      | Some (sha, _, _) when commit_has_links g sha -> i
      | _ -> find (i + 1)
  in find (from + 1)

let prev_linked_commit g from =
  let rec find i =
    if i < 0 then from
    else match List.nth_opt g.commits i with
      | Some (sha, _, _) when commit_has_links g sha -> i
      | _ -> find (i - 1)
  in find (from - 1)

let file_has_links g f =
  let sha = match List.nth_opt g.commits g.commit_idx with
    | Some (s, _, _) -> s | None -> "" in
  Hashtbl.mem g.git_links (sha, Filename.basename f)

let next_linked_file g from =
  let n = List.length g.files in
  let rec find i =
    if i >= n then from
    else match List.nth_opt g.files i with
      | Some f when file_has_links g f -> i
      | _ -> find (i + 1)
  in find (from + 1)

let prev_linked_file g from =
  let rec find i =
    if i < 0 then from
    else match List.nth_opt g.files i with
      | Some f when file_has_links g f -> i
      | _ -> find (i - 1)
  in find (from - 1)

let draw_git_left_panels ctx state =
  let size = LTerm_draw.size ctx in
  let g = state.git in
  let total_h = size.rows in
  let width = size.cols in
  let branch_items = List.map (fun b ->
    let prefix = if b = g.current_branch then "* " else "  " in
    (prefix ^ b, b = g.current_branch, false)
  ) g.branches in
  let commit_items = List.map (fun (sha, _ts, msg) ->
    let short_sha = if String.length sha >= 7 then String.sub sha 0 7 else sha in
    let w = max 1 (width - 10) in
    let msg_trunc = if String.length msg > w then String.sub msg 0 w else msg in
    let has_links = Hashtbl.fold (fun (s, _) _ found ->
      found || s = sha) g.git_links false in
    (Printf.sprintf " %s %s" short_sha msg_trunc, false, not has_links)
  ) g.commits in
  let cur_sha = match List.nth_opt g.commits g.commit_idx with
    | Some (sha, _, _) -> sha | None -> "" in
  let file_items = List.map (fun f ->
    let fb = Filename.basename f in
    let has_links = Hashtbl.mem g.git_links (cur_sha, fb) in
    let has_human = Hashtbl.mem g.human_edits (cur_sha, fb) in
    (* no Claude links = human edit; has_human = Claude + human mixed *)
    let is_human = not has_links || has_human in
    ("  " ^ f, is_human, false)
  ) g.files in
  let sorted_links = List.sort (fun (a : git_conv_link) (b : git_conv_link) ->
    let c = Int.compare a.turn_idx b.turn_idx in
    if c <> 0 then c else Int.compare a.entry_idx b.entry_idx
  ) g.link_candidates in
  let link_items = List.map (fun (link : git_conv_link) ->
    let is_human_only = let ek = link.edit_key in
      String.length ek > 11 &&
      String.sub ek (String.length ek - 11) 11 = ":human-only" in
    let is_human_mod = let ek = link.edit_key in
      String.length ek > 6 &&
      String.sub ek (String.length ek - 6) 6 = ":human" in
    let is_human = is_human_only || is_human_mod in
    if is_human_only then
      (" H human edit", true, false)
    else
      let sid = if String.length link.session_id > 4
        then String.sub link.session_id 0 4 else link.session_id in
      let marker = if is_human_mod then "H" else " " in
      (Printf.sprintf "%s%s t%d e%d" marker sid (link.turn_idx + 1) link.entry_idx,
       is_human, false)
  ) sorted_links in
  let panels = [
    ("Branches", g.focus = Branches, branch_items, g.branch_idx);
    ("Commits", g.focus = Commits, commit_items, g.commit_idx);
    ("Files", g.focus = Files, file_items, g.file_idx);
    (Printf.sprintf "Links (%d)" (List.length g.link_candidates),
     g.focus = Links, link_items, g.link_idx);
  ] in
  let n_panels = List.length panels in
  let focused_idx = match g.focus with
    | Branches -> 0 | Commits -> 1 | Files -> 2 | Links -> 3 in
  let focused_h = max 3 (total_h / 2) in
  let rest_h = max 0 (total_h - focused_h) in
  let other_h = if n_panels > 1 then max 2 (rest_h / (n_panels - 1)) else 0 in
  let heights = List.mapi (fun i _ ->
    if i = focused_idx then focused_h else other_h
  ) panels in
  let sum_h = List.fold_left (+) 0 heights in
  let heights = if sum_h <> total_h then
    let diff = total_h - sum_h in
    List.mapi (fun i h -> if i = focused_idx then h + diff else h) heights
  else heights in
  let row = ref 0 in
  List.iteri (fun i (title, focused, items, sel_idx) ->
    let h = List.nth heights i in
    let _ = draw_git_panel ctx ~row_start:!row ~height:h ~title ~focused
              ~items ~sel_idx ~width in
    row := !row + h
  ) panels

let draw_git_diff ctx state =
  let size = LTerm_draw.size ctx in
  let g = state.git in
  let header = match g.file_diff_filter with
    | Some f -> Printf.sprintf " Diff: %s " f
    | None -> " Diff " in
  draw_str ctx 0 0
    ~style:LTerm_style.{ none with background = Some c_status_bg;
                                    foreground = Some c_highlight; bold = Some true }
    (Printf.sprintf "%s%s" header
       (String.make (max 0 (size.cols - String.length header)) ' '));
  let diff_text = match g.file_diff_filter with
    | None -> g.diff_preview
    | Some file ->
      let lines = String.split_on_char '\n' g.diff_preview in
      let file_base = Filename.basename file in
      let in_section = ref false in
      let result = ref [] in
      List.iter (fun line ->
        if String.length line > 11 && String.sub line 0 10 = "diff --git" then begin
          let parts = String.split_on_char ' ' line in
          match List.rev parts with
          | last :: _ ->
            let f = if String.length last > 2 && last.[0] = 'b' && last.[1] = '/'
              then String.sub last 2 (String.length last - 2) else last in
            in_section := Filename.basename f = file_base
          | [] -> in_section := false
        end;
        if !in_section then result := line :: !result
      ) lines;
      String.concat "\n" (List.rev !result) in
  let lines = String.split_on_char '\n' diff_text in
  let parse_hunk line =
    try Scanf.sscanf line "@@ -%d,%d +%d,%d @@%_s"
          (fun o _ n _ -> Some (o, n))
    with _ ->
      try Scanf.sscanf line "@@ -%d +%d,%d @@%_s"
            (fun o n _ -> Some (o, n))
      with _ ->
        try Scanf.sscanf line "@@ -%d,%d +%d @@%_s"
              (fun o _ n -> Some (o, n))
        with _ -> None
  in
  let old_ln = ref 0 in
  let new_ln = ref 0 in
  let numbered = List.map (fun line ->
    if String.length line > 0 then match line.[0] with
      | '@' ->
        (match parse_hunk line with
         | Some (o, n) -> old_ln := o; new_ln := n
         | None -> ());
        (line, LTerm_style.cyan, c_bg)
      | '+' ->
        let prefix = Printf.sprintf "%4d " !new_ln in
        new_ln := !new_ln + 1;
        (prefix ^ line, c_diff_add_fg, c_diff_add_bg)
      | '-' ->
        let prefix = Printf.sprintf "%4d " !old_ln in
        old_ln := !old_ln + 1;
        (prefix ^ line, c_diff_del_fg, c_diff_del_bg)
      | _ ->
        let prefix = Printf.sprintf "%4d " !new_ln in
        old_ln := !old_ln + 1; new_ln := !new_ln + 1;
        (prefix ^ line, c_fg, c_bg)
    else ("", c_fg, c_bg)
  ) lines in
  let vh = max 0 (size.rows - 1) in
  let scroll = g.diff_scroll_git in
  let visible = List.filteri (fun i _ -> i >= scroll && i < scroll + vh) numbered in
  List.iteri (fun vi (text, fg, bg) ->
    let row = 1 + vi in
    if row < size.rows then
      draw_str ctx row 1
        ~style:LTerm_style.{ none with background = Some bg; foreground = Some fg }
        text
  ) visible

(* ---------- History mode drawing ---------- *)

let draw_history_results ctx state =
  let size = LTerm_draw.size ctx in
  let h = state.history in
  let title = Printf.sprintf " Search: \"%s\" (%d results)"
      h.search_query (List.length h.search_results) in
  draw_str ctx 0 0
    ~style:LTerm_style.{ none with background = Some c_status_bg;
                                    foreground = Some c_highlight; bold = Some true }
    (Printf.sprintf "%s%s" title (String.make (max 0 (size.cols - String.length title)) ' '));
  if h.search_results = [] then
    draw_str ctx 2 2
      ~style:LTerm_style.{ none with background = Some c_bg; foreground = Some c_border }
      "(no results)"
  else begin
    let row = ref 1 in
    List.iteri (fun i (session_id, user_text, _doc, _idx, _ts, distance) ->
      if !row < size.rows - 1 then begin
        let selected = i = h.result_idx in
        let marker = if selected then ">" else " " in
        let fg = if selected then c_highlight else c_fg in
        let bg = if selected then c_selection_bg else c_bg in
        let label = if user_text = "" then "(no text)" else
          let max_len = size.cols - 14 in
          if String.length user_text > max_len
          then String.sub user_text 0 max_len ^ "..."
          else user_text in
        let dist = Printf.sprintf "(%.2f)" distance in
        let line1 = Printf.sprintf " %s %s  %s" marker label dist in
        draw_str ctx !row 0
          ~style:LTerm_style.{ none with foreground = Some fg; background = Some bg;
                                          bold = Some selected }
          (if String.length line1 > size.cols then String.sub line1 0 size.cols
           else line1);
        incr row;
        if !row < size.rows - 1 then begin
          let sid = if String.length session_id > 8
            then String.sub session_id 0 8 else session_id in
          let line2 = Printf.sprintf "   session: %s" sid in
          draw_str ctx !row 0
            ~style:LTerm_style.{ none with foreground = Some c_border;
                                            background = Some bg }
            (if String.length line2 > size.cols then String.sub line2 0 size.cols
             else line2);
          incr row
        end
      end
    ) h.search_results
  end

let draw_history_content ctx state =
  let size = LTerm_draw.size ctx in
  let h = state.history in
  if h.showing_results then
    draw_history_results ctx state
  else if h.search_active then begin
    (* Show search input *)
    let title = " Search history" in
    draw_str ctx 0 0
      ~style:LTerm_style.{ none with background = Some c_status_bg;
                                      foreground = Some c_highlight; bold = Some true }
      (Printf.sprintf "%s%s" title (String.make (max 0 (size.cols - String.length title)) ' '));
    let prompt = Printf.sprintf " > %s_" h.search_query in
    draw_str ctx 2 0
      ~style:LTerm_style.{ none with foreground = Some c_highlight; background = Some c_bg }
      prompt
  end else begin
    let n_turns = List.length h.turns in
    let n_sessions = List.length h.sessions in
    let title = if n_sessions = 0 then " History (no sessions)"
      else
        let sid = match List.nth_opt h.sessions h.session_idx with
          | Some path -> Filename.basename path |> Filename.chop_extension
          | None -> "?" in
        let short_id = if String.length sid > 8 then String.sub sid 0 8 else sid in
        let result_info = if h.search_results <> [] && not h.showing_results then
          Printf.sprintf "  [result %d/%d, b=list, S-arrows=jump]"
            (h.result_idx + 1) (List.length h.search_results)
        else "" in
        Printf.sprintf " Session %s  Turn %d/%d  (session %d/%d)%s"
          short_id (h.turn_idx + 1) n_turns (h.session_idx + 1) n_sessions result_info in
    draw_str ctx 0 0
      ~style:LTerm_style.{ none with background = Some c_status_bg;
                                      foreground = Some c_highlight; bold = Some true }
      (Printf.sprintf "%s%s" title (String.make (max 0 (size.cols - String.length title)) ' '));
    let current_turn = List.nth_opt h.turns h.turn_idx in
    match current_turn with
    | None | Some [] ->
      draw_str ctx 2 2
        ~style:LTerm_style.{ none with background = Some c_bg; foreground = Some c_border }
        "(no conversation loaded)"
    | Some entries ->
      let width = max 1 (size.cols - 2) in
      let visible_entries = List.filteri (fun i _ -> i >= h.hist_scroll) entries in
      let all_lines =
        List.concat_map (render_entry ~width ~verbose:true) visible_entries in
      let visible = max 0 (size.rows - 1) in
      List.filteri (fun i _ -> i < visible) all_lines
      |> List.iteri (fun i line ->
        let row = 1 + i in
        if row < size.rows then
          draw_str ctx row 1
            ~style:LTerm_style.{ none with
              background = Some (match line.bg with Some b -> b | None -> c_bg);
              foreground = Some line.fg }
            line.text)
  end

(* ---------- Main draw ---------- *)

let draw_status_bar ctx size state =
  let sbg = LTerm_style.{ none with background = Some c_status_bg; foreground = Some c_fg } in
  draw_str ctx 0 0 ~style:sbg (String.make size.LTerm_geom.cols ' ');
  let mode_tag = match state.mode with
    | Conv -> "CONV" | Git -> "GIT" | History -> "HIST" in
  let model = if state.status_model = "" then "urme" else state.status_model in
  let ctx_str =
    if state.context_tokens > 0 then
      Printf.sprintf " %dk/200k" (state.context_tokens / 1000)
    else "" in
  let left = Printf.sprintf " [%s] %s%s" mode_tag model ctx_str in
  draw_str ctx 0 0
    ~style:LTerm_style.{ none with background = Some c_status_bg;
                                    foreground = Some c_highlight; bold = Some true }
    left;
  draw_str ctx 0
    (max (String.length left + 2)
       ((size.cols - String.length state.status_extra) / 2))
    ~style:sbg state.status_extra;
  let right_str = match state.config.plan, state.usage with
    | (Urme_core.Config.Pro | Urme_core.Config.Max), Some u ->
      let usage_str = Urme_core.Usage.format_status_bar u in
      if state.status_tokens_in > 0 || state.status_tokens_out > 0 then
        Printf.sprintf "%dk in / %dk out | %s"
          (state.status_tokens_in / 1000) (state.status_tokens_out / 1000)
          usage_str
      else usage_str
    | _ ->
      if state.status_tokens_in > 0 || state.status_tokens_out > 0 then
        Printf.sprintf "%dk in / %dk out | $%.4f"
          (state.status_tokens_in / 1000) (state.status_tokens_out / 1000)
          state.status_cost
      else ""
  in
  if right_str <> "" then
    draw_str ctx 0 (max 0 (size.cols - String.length right_str - 1)) ~style:sbg right_str

let draw_input_line ctx input_row size state =
  let ist = LTerm_style.{ none with background = Some c_input_bg; foreground = Some c_fg } in
  let prompt = match state.mode with
    | Conv -> (match state.pending with
      | Some p ->
        Printf.sprintf "[Y/N] Allow %s: %s? " p.tool_name
          (summarize_tool_input p.tool_name p.tool_input)
      | None -> if state.streaming then "..." else "> ")
    | Git -> " Tab/S-Tab=panel j/k=nav Enter=select [/]=scroll h=history Esc=back "
    | History ->
      let h = state.history in
      if h.showing_results && h.return_mode = Git then
        " Up/Down=select  Enter=view  Esc=back to git "
      else if h.showing_results then
        " Up/Down=select  Enter=view  /=new search  Esc=close "
      else if h.search_active then
        " type query...  Enter=search  Esc=cancel "
      else if h.return_mode = Git && h.search_results <> [] then
        " <-/->step  S-<-/S->=prev/next link  b=back to list  Esc=git  Up/Down=scroll "
      else if h.search_results <> [] then
        " <-/->step  S-<-/S->=prev/next result  b=back to list  /=search  q=exit "
      else
        " <-/->step  /=search  i=index  Up/Down=scroll  q=exit " in
  draw_str ctx input_row 0 ~style:ist
    (Printf.sprintf "%s%s%s" prompt state.input_text
       (String.make (max 0 (size.LTerm_geom.cols - String.length state.input_text
                            - String.length prompt)) ' '))

let draw_palette ctx input_row size state =
  if state.palette_open then begin
    let candidates = filter_commands state.input_text in
    let n = List.length candidates in
    if n > 0 then begin
      let max_show = min n 6 in
      let box_width = min (size.LTerm_geom.cols - 2) 50 in
      let box_top = max 1 (input_row - max_show) in
      let sel_bg = LTerm_style.rgb 50 50 90 in
      let norm_bg = LTerm_style.rgb 35 35 60 in
      let sel_style = LTerm_style.{ none with background = Some sel_bg;
                                               foreground = Some c_highlight;
                                               bold = Some true } in
      let norm_style = LTerm_style.{ none with background = Some norm_bg;
                                                foreground = Some c_fg } in
      let sel = state.palette_selected mod n in
      let scroll_off =
        if sel < max_show then 0
        else sel - max_show + 1 in
      List.iteri (fun i c ->
        let vi = i - scroll_off in
        if vi >= 0 && vi < max_show then begin
          let row = box_top + vi in
          let selected = i = sel in
          let style = if selected then sel_style else norm_style in
          let label = Printf.sprintf " %-12s %s" c.cmd c.description in
          let label = if String.length label >= box_width
            then String.sub label 0 (box_width - 1) else label in
          let padded = label ^ String.make (max 0 (box_width - String.length label)) ' ' in
          draw_str ctx row 1 ~style padded
        end
      ) candidates
    end
  end

let draw mvar ui matrix =
  match peek mvar with
  | None -> ()
  | Some state ->
    let size = LTerm_ui.size ui in
    let ctx = LTerm_draw.context matrix size in
    LTerm_draw.clear ctx;
    LTerm_draw.fill_style ctx LTerm_style.{ none with background = Some c_bg };
    let input_row = size.rows - 1 in
    let pane_height = max 1 (size.rows - 2) in
    (* Shared: status bar *)
    draw_status_bar ctx size state;
    (* Mode-specific content *)
    (match state.mode with
     | Conv ->
       if pane_height > 0 then
         draw_conversation
           (LTerm_draw.sub ctx LTerm_geom.{ row1=1; col1=0; row2=1+pane_height; col2=size.cols })
           state
     | Git ->
       let left_w = max 1 (size.cols / 4) in
       let bs = LTerm_style.{ none with background = Some c_bg; foreground = Some c_border } in
       if left_w > 0 && pane_height > 0 then
         draw_git_left_panels
           (LTerm_draw.sub ctx { row1=1; col1=0; row2=1+pane_height; col2=left_w })
           state;
       if left_w > 0 && left_w < size.cols then
         for row = 1 to pane_height do draw_str ctx row left_w ~style:bs "\xe2\x94\x82" done;
       if size.cols > left_w + 1 && pane_height > 0 then
         draw_git_diff
           (LTerm_draw.sub ctx { row1=1; col1=left_w+1; row2=1+pane_height; col2=size.cols })
           state
     | History ->
       if pane_height > 0 then
         draw_history_content
           (LTerm_draw.sub ctx LTerm_geom.{ row1=1; col1=0; row2=1+pane_height; col2=size.cols })
           state);
    (* Shared: input line *)
    draw_input_line ctx input_row size state;
    (match state.mode with
     | Conv ->
       LTerm_ui.set_cursor_visible ui true;
       LTerm_ui.set_cursor_position ui
         LTerm_geom.{ row = input_row;
                      col = (let prompt_len = match state.pending with
                        | Some p -> String.length (Printf.sprintf "[Y/N] Allow %s: %s? " p.tool_name
                            (summarize_tool_input p.tool_name p.tool_input))
                        | None -> if state.streaming then 3 else 2 in
                      prompt_len + String.length state.input_text) }
     | _ -> LTerm_ui.set_cursor_visible ui false);
    (* Shared: palette overlay *)
    draw_palette ctx input_row size state

(* ---------- Daemon ---------- *)

let ensure_daemon mvar =
  let* s = Lwt_mvar.take mvar in
  match s.daemon with
  | Some d when Urme_claude.Process.is_running d ->
    let* () = Lwt_mvar.put mvar s in
    Lwt.return d
  | _ ->
    let bridge = find_bridge_binary () in
    let sock = match s.perm_server with
      | Some ps -> Some (Permission_server.path ps) | None -> None in
    let opts = { Urme_claude.Process.default_opts with
      permission_bridge_binary = Some bridge;
      permission_socket_path = sock } in
    let* d = Urme_claude.Process.spawn ~cwd:s.project_dir ~opts
        ~binary:s.config.Urme_core.Config.claude_binary () in
    let* () = Lwt_mvar.put mvar { s with daemon = Some d } in
    Lwt.return d

(* ---------- Commit-centric git ↔ conversation link index ---------- *)

let log_debug msg =
  let oc = open_out_gen [Open_append; Open_creat] 0o644 "/tmp/urme_debug.log" in
  Printf.fprintf oc "[%s] %s\n" (string_of_float (Unix.gettimeofday ())) msg;
  close_out oc

let parse_git_info = Urme_engine.Git_link_types.parse_git_info_json

(* update_git_links: delegate to Git_link engine module *)
let update_git_links ~project_dir ~port ~collection_id ?(sessions_filter : string list option) mvar =
  let on_status msg =
    let* () = update mvar (fun s -> { s with status_extra = msg }) in
    redraw mvar; Lwt.return_unit in
  Urme_engine.Git_link.update_index ~project_dir ~port ~collection_id
    ?sessions_filter ~on_status ()

(* ---------- Mode switching helpers ---------- *)

let load_git_data ~cwd =
  let errs = ref [] in
  let p_branches = Lwt.catch
    (fun () -> Urme_git.Ops.run_git ~cwd ["branch"; "--list"; "--no-color"])
    (fun exn -> errs := ("branches: " ^ Printexc.to_string exn) :: !errs; Lwt.return "") in
  let p_cur = Lwt.catch
    (fun () -> Urme_git.Ops.current_branch ~cwd)
    (fun exn -> errs := ("cur_branch: " ^ Printexc.to_string exn) :: !errs; Lwt.return "") in
  let p_commits = Lwt.catch
    (fun () -> Urme_git.Ops.walk_log ~cwd ~max_count:200 ())
    (fun exn -> errs := ("commits: " ^ Printexc.to_string exn) :: !errs; Lwt.return []) in
  let* branches_raw = p_branches
  and* cur = p_cur
  and* commits = p_commits in
  let branches = String.split_on_char '\n' branches_raw
    |> List.filter_map (fun s ->
      let s = String.trim s in
      if s = "" then None
      else Some (if String.length s > 2 && s.[0] = '*' then
        String.trim (String.sub s 2 (String.length s - 2)) else s)) in
  (* diff + files depend on commits, but are independent of each other *)
  let p_diff = match commits with
    | (sha, _, _) :: _ ->
      Lwt.catch (fun () -> Urme_git.Ops.commit_diff ~cwd ~sha) (fun _ -> Lwt.return "")
    | [] -> Lwt.return "" in
  let p_files = match commits with
    | (sha, _, _) :: _ ->
      Lwt.catch (fun () -> Urme_git.Ops.commit_changed_files ~cwd ~sha) (fun _ -> Lwt.return [])
    | [] -> Lwt.return [] in
  let* diff_preview = p_diff
  and* files = p_files in
  let debug = Printf.sprintf "cwd=%s branches=%d commits=%d files=%d errs=[%s]"
    cwd (List.length branches) (List.length commits) (List.length files)
    (String.concat "; " !errs) in
  Lwt.return ({ branches; current_branch = cur; commits; files;
               focus = Branches; branch_idx = 0; commit_idx = 0; file_idx = 0;
               link_idx = 0; diff_preview; diff_scroll_git = 0;
               file_diff_filter = None; git_links = Hashtbl.create 0;
               human_edits = Hashtbl.create 0;
               link_candidates = [] }, debug)

(* Rebuild in-memory git_links from ChromaDB's git_info metadata *)
let load_git_links_from_chroma ~port ~project =
  Lwt.catch (fun () ->
    let* collection_id =
      Urme_search.Chromadb.ensure_interactions_collection ~port ~project in
    let* existing_gis =
      Urme_search.Chromadb.get_all_with_git_info ~port ~collection_id in
    let links : (string * string, git_conv_link list) Hashtbl.t =
      Hashtbl.create 256 in
    List.iter (fun (_id, gi_str, session_id, _idx, _ts) ->
      let tbl = parse_git_info gi_str in
      Hashtbl.iter (fun ek value ->
        match value with
        | Some (gi : Urme_engine.Git_link_types.git_info) ->
          let file_base = match String.split_on_char ':' ek with
            | fb :: _ -> fb | [] -> "" in
          if file_base <> "" then begin
            let lk = (gi.commit_sha, file_base) in
            let existing = match Hashtbl.find_opt links lk with
              | Some l -> l | None -> [] in
            if not (List.exists (fun (l : git_conv_link) -> l.edit_key = ek) existing) then begin
              let link : git_conv_link = { commit_sha = gi.commit_sha; file = file_base;
                           session_id; turn_idx = gi.turn_idx; entry_idx = gi.entry_idx;
                           edit_key = ek } in
              Hashtbl.replace links lk (existing @ [link])
            end
          end
        | None -> ()
      ) tbl
    ) existing_gis;
    Lwt.return links
  ) (fun _ -> Lwt.return (Hashtbl.create 0))

let switch_to_git mvar =
  let* s = Lwt_mvar.take mvar in
  let* () = Lwt_mvar.put mvar { s with status_extra = "Loading git data..." } in
  redraw mvar;
  let* s = Lwt_mvar.take mvar in
  let* (git, debug) = load_git_data ~cwd:s.project_dir in
  (* If in-memory git_links are empty, start ChromaDB if needed and load *)
  let* git_links =
    if Hashtbl.length s.git.git_links > 0 then
      Lwt.return s.git.git_links
    else begin
      let port = s.config.chromadb_port in
      let* () = Lwt_mvar.put mvar { s with status_extra = "Starting ChromaDB..." } in
      redraw mvar;
      let start_chroma () =
        let chroma_dir = Filename.concat s.project_dir "chroma" in
        (try Unix.mkdir chroma_dir 0o755 with Unix.Unix_error (Unix.EEXIST, _, _) -> ());
        ignore (Sys.command (Printf.sprintf
          "chroma run --port %d --path %s > /tmp/chromadb.log 2>&1 &" port chroma_dir));
        let rec wait n =
          if n <= 0 then Lwt.return_unit
          else Lwt.catch (fun () ->
            let uri = Uri.of_string (Printf.sprintf "http://[::1]:%d/api/v2/heartbeat" port) in
            let* _resp, body = Cohttp_lwt_unix.Client.get uri in
            let* _ = Cohttp_lwt.Body.to_string body in
            Lwt.return_unit
          ) (fun _ -> let* () = Lwt_unix.sleep 1.0 in wait (n - 1))
        in wait 10
      in
      let* () = Lwt.catch (fun () ->
        let uri = Uri.of_string (Printf.sprintf "http://[::1]:%d/api/v2/heartbeat" port) in
        let* _resp, body = Cohttp_lwt_unix.Client.get uri in
        let* _ = Cohttp_lwt.Body.to_string body in
        Lwt.return_unit
      ) (fun _ -> start_chroma ()) in
      let* s = Lwt_mvar.take mvar in
      let* () = Lwt_mvar.put mvar { s with status_extra = "Loading git links..." } in
      redraw mvar;
      let* links = load_git_links_from_chroma ~port
          ~project:(Filename.basename s.project_dir) in
      let* s = Lwt_mvar.take mvar in
      ignore s; Lwt.return links
    end in
  let git = { git with git_links } in
  let status = if git.branches = [] then
    Printf.sprintf "Git: no branches! %s" debug
  else Printf.sprintf "Git browser (%d link keys)" (Hashtbl.length git_links) in
  let* () = Lwt_mvar.put mvar { s with mode = Git; git; status_extra = status } in
  redraw mvar; Lwt.return_unit

(* Check if a service is reachable *)
let check_port port =
  Lwt.catch (fun () ->
    let uri = Uri.of_string (Printf.sprintf "http://[::1]:%d/api/v2/heartbeat" port) in
    let* resp, body = Cohttp_lwt_unix.Client.get uri in
    let* _ = Cohttp_lwt.Body.to_string body in
    Lwt.return (Cohttp.Response.status resp |> Cohttp.Code.code_of_status = 200)
  ) (fun _ -> Lwt.return false)

let check_ollama url =
  Lwt.catch (fun () ->
    let uri = Uri.of_string (url ^ "/api/tags") in
    let* resp, body = Cohttp_lwt_unix.Client.get uri in
    let* _ = Cohttp_lwt.Body.to_string body in
    Lwt.return (Cohttp.Response.status resp |> Cohttp.Code.code_of_status = 200)
  ) (fun _ -> Lwt.return false)

(* Start services if not running, wait for them to be ready.
   Starts services if not already running. *)
let ensure_services mvar ~chromadb_port ~ollama_url ~project_dir =
  let* chroma_ok = check_port chromadb_port in
  let* did_start_chroma = if not chroma_ok then begin
    let* () = update mvar (fun s -> { s with status_extra = "Starting ChromaDB..." }) in
    redraw mvar;
    let chroma_dir = Filename.concat project_dir "chroma" in
    (try Unix.mkdir chroma_dir 0o755 with Unix.Unix_error (Unix.EEXIST, _, _) -> ());
    let cmd = Printf.sprintf
      "chroma run --port %d --path %s > /tmp/chromadb.log 2>&1 &"
      chromadb_port chroma_dir in
    ignore (Sys.command cmd);
    let rec wait n =
      if n <= 0 then Lwt.return false
      else
        let* ok = check_port chromadb_port in
        if ok then Lwt.return true
        else let* () = Lwt_unix.sleep 1.0 in wait (n - 1)
    in
    wait 10
  end else Lwt.return false in
  let* ollama_ok = check_ollama ollama_url in
  let* did_start_ollama = if not ollama_ok then begin
    let* () = update mvar (fun s -> { s with status_extra = "Starting Ollama..." }) in
    redraw mvar;
    ignore (Sys.command "ollama serve > /tmp/ollama.log 2>&1 &");
    let rec wait n =
      if n <= 0 then Lwt.return false
      else
        let* ok = check_ollama ollama_url in
        if ok then Lwt.return true
        else let* () = Lwt_unix.sleep 1.0 in wait (n - 1)
    in
    wait 10
  end else Lwt.return false in
  (* Track what we started *)
  let* () = update mvar (fun s ->
    { s with started_chroma = s.started_chroma || did_start_chroma;
             started_ollama = s.started_ollama || did_start_ollama }) in
  Lwt.return_unit

(* Extract individual turns (user_text, assistant_text) from a session JSONL *)
let session_turns filepath =
  let turns = ref [] in
  let current_user = ref "" in
  let assistant_buf = Buffer.create 1024 in
  let flush_turn () =
    if !current_user <> "" && Buffer.length assistant_buf > 0 then
      turns := (!current_user, Buffer.contents assistant_buf) :: !turns;
    current_user := "";
    Buffer.clear assistant_buf in
  let ic = open_in filepath in
  (try while true do
    let line = input_line ic in
    (try
      let json = Yojson.Safe.from_string line in
      let open Yojson.Safe.Util in
      let typ = json |> member "type" |> to_string_option
                |> Option.value ~default:"" in
      (match typ with
       | "user" ->
         flush_turn ();
         let content = json |> member "message" |> member "content" in
         (match content with
          | `String s ->
            if not (is_internal_noise s) then
              current_user := (strip_xml_tags s)
          | `List items ->
            List.iter (fun item ->
              match item |> member "type" |> to_string_option with
              | Some "text" ->
                let t = item |> member "text" |> to_string_option
                        |> Option.value ~default:"" in
                if t <> "" && not (is_internal_noise t) then
                  current_user := (strip_xml_tags t)
              | _ -> ()
            ) items
          | _ -> ())
       | "assistant" ->
         let blocks = json |> member "message" |> member "content"
           |> (fun j -> try to_list j with _ -> []) in
         List.iter (fun block ->
           match block |> member "type" |> to_string_option with
           | Some "text" ->
             let t = block |> member "text" |> to_string_option
                     |> Option.value ~default:"" in
             if t <> "" then begin
               if Buffer.length assistant_buf > 0 then
                 Buffer.add_char assistant_buf '\n';
               Buffer.add_string assistant_buf t
             end
           | _ -> ()
         ) blocks
       | _ -> ())
     with _ -> ())
  done with End_of_file -> ());
  flush_turn ();
  close_in ic;
  List.rev !turns

(* Index all session JSONL files into ChromaDB — incremental with git correlation *)
let index_sessions mvar =
  let* () = update mvar (fun s ->
    { s with status_extra = "Starting services..." }) in
  redraw mvar;
  Lwt.async (fun () ->
    Lwt.catch (fun () ->
      let* s_val = Lwt_mvar.take mvar in
      let* () = Lwt_mvar.put mvar s_val in
      let port = s_val.config.chromadb_port in
      let ollama_url = s_val.config.ollama_url in
      let project = Filename.basename s_val.project_dir in
      let cwd = s_val.project_dir in
      let* () = ensure_services mvar ~chromadb_port:port ~ollama_url
          ~project_dir:cwd in
      let* () = update mvar (fun s -> { s with status_extra = "Checking index..." }) in
      redraw mvar;
      let* collection_id =
        Urme_search.Chromadb.ensure_interactions_collection ~port ~project in
      (* Batch-fetch all existing IDs in one call *)
      let* existing_ids =
        Urme_search.Chromadb.get_all_interaction_ids ~port ~collection_id in
      let existing_set = Hashtbl.create (List.length existing_ids) in
      List.iter (fun id -> Hashtbl.replace existing_set id ()) existing_ids;
      let jsonl_dir = Urme_search.Jsonl_reader.find_jsonl_dir
          ~project_dir:cwd in
      let sessions = sort_by_size_desc
        (Urme_search.Jsonl_reader.list_sessions ~jsonl_dir) in
      let total = List.length sessions in
      let n_new = ref 0 in
      let n_skip = ref 0 in
      let indexed_count = ref 0 in
      (* Collect all new interactions, then batch-save *)
      let pending = ref [] in
      List.iter (fun filepath ->
        let session_id = Filename.basename filepath |> Filename.chop_extension in
        let interactions = Urme_search.Jsonl_reader.parse_interactions ~filepath in
        if interactions <> [] then begin
          incr indexed_count;
          List.iteri (fun i (interaction : Urme_core.Types.interaction) ->
            let id = Printf.sprintf "%s_%d" session_id i in
            if Hashtbl.mem existing_set id then
              incr n_skip
            else begin
              incr n_new;
              pending := (id, session_id, i,
                interaction.user_text, interaction.assistant_summary,
                interaction.user_uuid, interaction.timestamp,
                interaction.files_changed) :: !pending
            end
          ) interactions
        end
      ) sessions;
      let all_pending = List.rev !pending in
      let batch_size = 64 in
      let batches = ref [] in
      let cur = ref [] in
      let cur_n = ref 0 in
      List.iter (fun item ->
        cur := item :: !cur;
        incr cur_n;
        if !cur_n >= batch_size then begin
          batches := List.rev !cur :: !batches;
          cur := []; cur_n := 0
        end
      ) all_pending;
      if !cur <> [] then batches := List.rev !cur :: !batches;
      let all_batches = List.rev !batches in
      let n_batches = List.length all_batches in
      let batch_i = ref 0 in
      let* () = Lwt_list.iter_s (fun batch ->
        incr batch_i;
        let* () = update mvar (fun s ->
          { s with status_extra =
              Printf.sprintf "Indexing batch %d/%d (%d new, %d skip, %d sessions)"
                !batch_i n_batches !n_new !n_skip total }) in
        redraw mvar;
        Lwt.catch (fun () ->
          Urme_search.Chromadb.save_interactions_batch ~port ~collection_id batch
        ) (fun exn ->
          let oc = open_out_gen [Open_append; Open_creat] 0o644 "/tmp/urme_debug.log" in
          Printf.fprintf oc "[%.2f] Batch %d FAILED: %s\n%!" (Unix.gettimeofday ()) !batch_i
            (Printexc.to_string exn);
          close_out oc;
          Lwt.return_unit)
      ) all_batches in
      (* Find sessions with interactions missing git_info *)
      let* all_ids =
        Urme_search.Chromadb.get_all_interaction_ids ~port ~collection_id in
      let* gis =
        Urme_search.Chromadb.get_all_with_git_info ~port ~collection_id in
      let has_gi = Hashtbl.create (List.length gis) in
      List.iter (fun (id, _, _, _, _) -> Hashtbl.replace has_gi id ()) gis;
      let sessions_needing_scan = Hashtbl.create 16 in
      List.iter (fun id ->
        if not (Hashtbl.mem has_gi id) then
          (* id = "session_id_index" — extract session_id *)
          match String.rindex_opt id '_' with
          | Some pos -> Hashtbl.replace sessions_needing_scan
              (String.sub id 0 pos) ()
          | None -> ()
      ) all_ids;
      let sids = Hashtbl.fold (fun k () acc -> k :: acc) sessions_needing_scan [] in
      let n_scan = List.length sids in
      if n_scan = 0 && !n_new = 0 then begin
        (* Rebuild in-memory links from ChromaDB *)
        let* git_links = load_git_links_from_chroma ~port ~project in
        let n_links = Hashtbl.length git_links in
        let* () = update mvar (fun s ->
          { s with git = { s.git with git_links };
                   status_extra =
              Printf.sprintf "Up to date: %d existing, %d links (%d sessions)"
                !n_skip n_links total }) in
        redraw mvar; Lwt.return_unit
      end else begin
        let* () = update mvar (fun s ->
          { s with status_extra =
              Printf.sprintf "Git links: scanning %d sessions (%d new)..."
                n_scan !n_new }) in
        redraw mvar;
        let* (git_links, human_edits, gl_edits, gl_matched, _gl_commits, _gl_relinked) =
          update_git_links ~project_dir:cwd ~port ~collection_id
            ~sessions_filter:sids mvar in
        let n_links = Hashtbl.length git_links in
        let* () = update mvar (fun s ->
          { s with git = { s.git with git_links; human_edits };
                   status_extra =
              Printf.sprintf "Done: %d new, %d scanned | %d edits, %d matched, %d links"
                !n_new n_scan gl_edits gl_matched n_links }) in
        redraw mvar; Lwt.return_unit
      end
    ) (fun exn ->
      let* () = update mvar (fun s ->
        { s with status_extra = Printf.sprintf "Index error: %s"
            (Printexc.to_string exn) }) in
      redraw mvar; Lwt.return_unit));
  Lwt.return_unit

let execute_search mvar query =
  let* () = update mvar (fun s ->
    { s with status_extra = "Starting services..." ;
             history = { s.history with search_active = false } }) in
  redraw mvar;
  Lwt.async (fun () ->
    Lwt.catch (fun () ->
      let* s_val = Lwt_mvar.take mvar in
      let* () = Lwt_mvar.put mvar s_val in
      let port = s_val.config.chromadb_port in
      let ollama_url = s_val.config.ollama_url in
      let project = Filename.basename s_val.project_dir in
      let* () = ensure_services mvar ~chromadb_port:port ~ollama_url
          ~project_dir:s_val.project_dir in
      let* () = update mvar (fun s -> { s with status_extra = "Searching..." }) in
      redraw mvar;
      let* collection_id =
        Urme_search.Chromadb.ensure_interactions_collection ~port ~project in
      let* results = Urme_search.Chromadb.search_all_interactions ~port
          ~collection_id ~query ~n:20 in
      let* () = update mvar (fun s ->
        { s with history = { s.history with
            search_results = results;
            showing_results = true;
            search_query = query;
            result_idx = 0 };
          status_extra = Printf.sprintf "Found %d results" (List.length results) }) in
      redraw mvar; Lwt.return_unit
    ) (fun exn ->
      let* () = update mvar (fun s ->
        { s with history = { s.history with showing_results = false };
                 status_extra = Printf.sprintf "Search error: %s"
                   (Printexc.to_string exn) }) in
      redraw mvar; Lwt.return_unit));
  Lwt.return_unit

let switch_to_history mvar =
  let* () = update mvar (fun s ->
    let jsonl_dir = Urme_search.Jsonl_reader.find_jsonl_dir
        ~project_dir:s.project_dir in
    let sessions = Urme_search.Jsonl_reader.list_sessions ~jsonl_dir in
    let turns = match sessions with
      | path :: _ -> split_into_turns (load_session_entries path)
      | [] -> [] in
    let n = List.length sessions in
    { s with mode = History;
             history = { sessions; session_idx = 0; turns; turn_idx = 0;
                         hist_scroll = 0; return_mode = s.mode;
                         search_active = false; search_query = "";
                         search_results = []; result_idx = 0;
                         showing_results = false };
             status_extra = Printf.sprintf "History (%d sessions)" n }) in
  redraw mvar; Lwt.return_unit

(* ---------- Slash commands ---------- *)

let handle_command mvar cmd =
  let parts = String.split_on_char ' ' cmd in
  let name = List.hd parts in
  let _args = List.tl parts in
  let args_str = String.concat " " _args in
  match name with
  | "/clear" | "/new" ->
    let* () = update mvar (fun s ->
      { s with entries = []; diff_text = ""; diff_scroll = 0;
               scroll_offset = 0; stream_text = "";
               status_extra = "Cleared." }) in
    redraw mvar; Lwt.return true

  | "/reset" | "/clearall" ->
    let* s = Lwt_mvar.take mvar in
    (match s.daemon with Some d -> Urme_claude.Process.kill d | None -> ());
    let* () = Lwt_mvar.put mvar
      { s with entries = []; diff_text = ""; diff_scroll = 0;
               scroll_offset = 0; stream_text = ""; daemon = None;
               tool_use_map = []; status_tokens_in = 0;
               status_tokens_out = 0; status_cost = 0.0;
               status_extra = "Session reset." } in
    redraw mvar; Lwt.return true

  | "/verbose" | "/v" ->
    let* () = update mvar (fun s ->
      { s with verbose = not s.verbose;
               status_extra = if s.verbose then "Verbose off" else "Verbose on" }) in
    redraw mvar; Lwt.return true

  | "/compact" ->
    let* s = Lwt_mvar.take mvar in
    (match s.daemon with
     | Some d when Urme_claude.Process.is_running d ->
       let msg = if args_str <> "" then
         Printf.sprintf "Compact the conversation, focusing on: %s" args_str
       else "Summarize our conversation so far to free up context" in
       let* () = Lwt_mvar.put mvar { s with status_extra = "Compacting..." } in
       let* () = Urme_claude.Process.send d ~text:msg in
       redraw mvar; Lwt.return true
     | _ ->
       let* () = Lwt_mvar.put mvar { s with status_extra = "No active session" } in
       redraw mvar; Lwt.return true)

  | "/cost" ->
    let* () = update mvar (fun s ->
      let usage_str = match s.config.plan, s.usage with
        | (Urme_core.Config.Pro | Urme_core.Config.Max), Some u ->
          Urme_core.Usage.format_detailed u
        | _ ->
          Printf.sprintf "Cost: $%.4f" s.status_cost
      in
      let info = Printf.sprintf
        "Tokens in: %d (%dk) | Tokens out: %d (%dk)\n%s"
        s.status_tokens_in (s.status_tokens_in / 1000)
        s.status_tokens_out (s.status_tokens_out / 1000)
        usage_str in
      { s with entries = System_info info :: s.entries }) in
    redraw mvar; Lwt.return true

  | "/diff" ->
    let* () = update mvar (fun s ->
      { s with active_pane = `Diff; diff_scroll = 0 }) in
    redraw mvar; Lwt.return true

  | "/model" ->
    if args_str <> "" then begin
      (* Switch model by passing to daemon *)
      let* s = Lwt_mvar.take mvar in
      match s.daemon with
      | Some d when Urme_claude.Process.is_running d ->
        let* () = Lwt_mvar.put mvar
          { s with status_extra = Printf.sprintf "Model: %s" args_str } in
        let* () = Urme_claude.Process.send d
          ~text:(Printf.sprintf "Please switch to model %s" args_str) in
        redraw mvar; Lwt.return true
      | _ ->
        let* () = Lwt_mvar.put mvar
          { s with status_extra = "No active session"; status_model = args_str } in
        redraw mvar; Lwt.return true
    end else begin
      let* () = update mvar (fun s ->
        let info = Printf.sprintf "Current model: %s"
          (if s.status_model = "" then "(not connected)" else s.status_model) in
        { s with entries = System_info info :: s.entries }) in
      redraw mvar; Lwt.return true
    end

  | "/plan" ->
    let* s = Lwt_mvar.take mvar in
    (match s.daemon with
     | Some d when Urme_claude.Process.is_running d ->
       let* () = Lwt_mvar.put mvar
         { s with status_extra = "Plan mode..." } in
       let* () = Urme_claude.Process.send d
         ~text:"Enter plan mode. Before making any changes, outline your plan and wait for my approval." in
       redraw mvar; Lwt.return true
     | _ ->
       let* () = Lwt_mvar.put mvar { s with status_extra = "No active session" } in
       redraw mvar; Lwt.return true)

  | "/status" ->
    let* () = update mvar (fun s ->
      let running = match s.daemon with
        | Some d -> Urme_claude.Process.is_running d | None -> false in
      let cost_line = match s.config.plan, s.usage with
        | (Urme_core.Config.Pro | Urme_core.Config.Max), Some u ->
          Urme_core.Usage.format_detailed u
        | _ -> Printf.sprintf "Cost: $%.4f" s.status_cost
      in
      let info = Printf.sprintf
        "Model: %s\nDaemon: %s\nProject: %s\nTokens: %dk in / %dk out\n%s"
        (if s.status_model = "" then "(none)" else s.status_model)
        (if running then "running" else "stopped")
        s.project_dir
        (s.status_tokens_in / 1000) (s.status_tokens_out / 1000)
        cost_line in
      { s with entries = System_info info :: s.entries }) in
    redraw mvar; Lwt.return true

  | "/help" ->
    let help =
      List.map (fun c -> Printf.sprintf "  %-12s %s" c.cmd c.description) (Lazy.force commands)
      |> String.concat "\n" in
    let help = help ^ "\n\n  Keybindings:\n\
       \  Tab          Switch pane focus\n\
       \  Up/Down      Scroll active pane\n\
       \  Ctrl-V       Toggle thinking blocks\n\
       \  Ctrl-C       Quit" in
    let* () = update mvar (fun s ->
      { s with entries = System_info help :: s.entries }) in
    redraw mvar; Lwt.return true

  | "/agents" ->
    let* () = update mvar (fun s ->
      let info = match s.daemon with
        | Some d when Urme_claude.Process.is_running d ->
          Printf.sprintf "Claude daemon: running (model: %s)"
            (if s.status_model = "" then "unknown" else s.status_model)
        | _ -> "No agents running" in
      { s with entries = System_info info :: s.entries }) in
    redraw mvar; Lwt.return true

  | "/context" ->
    let* () = update mvar (fun s ->
      let total_chars = List.fold_left (fun acc e -> acc + match e with
        | User_msg t | Assistant_text t | Thinking_block t | System_info t -> String.length t
        | Tool_use_block { inline_diff; _ } ->
          (match inline_diff with Some d -> String.length d | None -> 0)
        | Tool_result_block { content; _ } -> String.length content
        | Turn_separator -> 0
      ) 0 s.entries in
      let est_tokens = total_chars / 4 in
      let info = Printf.sprintf
        "Entries: %d | ~%dk tokens used | %dk in / %dk out"
        (List.length s.entries) (est_tokens / 1000)
        (s.status_tokens_in / 1000) (s.status_tokens_out / 1000) in
      { s with entries = System_info info :: s.entries }) in
    redraw mvar; Lwt.return true

  | "/copy" ->
    let* s = Lwt_mvar.take mvar in
    let last_text = List.find_map (fun e -> match e with
      | Assistant_text t -> Some t | _ -> None) s.entries in
    (match last_text with
     | Some t ->
       let* _status = Lwt_process.exec
         (Lwt_process.shell (Printf.sprintf "printf '%%s' %s | pbcopy"
            (Filename.quote t))) in
       let* () = Lwt_mvar.put mvar { s with status_extra = "Copied to clipboard" } in
       redraw mvar; Lwt.return true
     | None ->
       let* () = Lwt_mvar.put mvar { s with status_extra = "No response to copy" } in
       redraw mvar; Lwt.return true)

  | "/export" ->
    let* s_val = Lwt_mvar.take mvar in
    let buf = Buffer.create 1024 in
    List.iter (fun e -> match e with
      | User_msg t -> Buffer.add_string buf (Printf.sprintf "User: %s\n\n" t)
      | Assistant_text t -> Buffer.add_string buf (Printf.sprintf "Assistant: %s\n\n" t)
      | Thinking_block t -> Buffer.add_string buf (Printf.sprintf "[Thinking] %s\n\n" t)
      | Tool_use_block { tool_name; inline_diff; _ } ->
        Buffer.add_string buf (Printf.sprintf "[%s]\n" tool_name);
        (match inline_diff with Some d -> Buffer.add_string buf (d ^ "\n") | None -> ())
      | Tool_result_block { tool_name; content; _ } ->
        Buffer.add_string buf (Printf.sprintf "[%s result] %s\n\n" tool_name content)
      | System_info t -> Buffer.add_string buf (Printf.sprintf "[System] %s\n\n" t)
      | Turn_separator -> Buffer.add_string buf "---\n\n"
    ) (List.rev s_val.entries);
    let filename = if args_str <> "" then args_str
      else Printf.sprintf "urme-export-%s.txt"
        (string_of_float (Unix.gettimeofday ())) in
    (try
       let oc = open_out filename in
       output_string oc (Buffer.contents buf);
       close_out oc
     with _ -> ());
    let* () = Lwt_mvar.put mvar
      { s_val with entries = System_info (Printf.sprintf "Exported to %s" filename)
                             :: s_val.entries } in
    redraw mvar; Lwt.return true

  | "/login" ->
    let* () = update mvar (fun s ->
      { s with entries = System_info "Run 'claude login' in your terminal to authenticate." :: s.entries }) in
    redraw mvar; Lwt.return true

  | "/skills" ->
    let* () = update mvar (fun s ->
      { s with entries = System_info "No custom skills configured." :: s.entries }) in
    redraw mvar; Lwt.return true

  | "/git" ->
    let* () = switch_to_git mvar in
    Lwt.return true

  | "/history" ->
    let* () = switch_to_history mvar in
    let* () = if args_str <> "" then execute_search mvar args_str
      else Lwt.return_unit in
    Lwt.return true

  | "/exit" | "/quit" ->
    (* Signal exit — handled specially in the event loop *)
    Lwt.return false

  | _ when String.length name > 0 && name.[0] = '/' ->
    let* () = update mvar (fun s ->
      { s with entries = System_info (Printf.sprintf "Unknown command: %s. Type /help for available commands." name)
                         :: s.entries }) in
    redraw mvar; Lwt.return true
  | _ -> Lwt.return false

(* ---------- Usage refresh ---------- *)

let refresh_usage mvar =
  Lwt.async (fun () ->
    Lwt.catch (fun () ->
      let* result = Urme_core.Usage.fetch () in
      match result with
      | Some u ->
        let* () = update mvar (fun s -> { s with usage = Some u }) in
        redraw mvar; Lwt.return_unit
      | None -> Lwt.return_unit
    ) (fun _exn -> Lwt.return_unit))

let maybe_refresh_usage mvar s =
  let count = s.message_count + 1 in
  let s = { s with message_count = count } in
  (match s.config.plan with
   | Urme_core.Config.Pro | Urme_core.Config.Max ->
     if count = 1 || count mod 10 = 0 then refresh_usage mvar
   | Urme_core.Config.Api -> ());
  s

(* ---------- Auto-index previous turn ---------- *)

let extract_last_turn entries =
  (* entries are newest-first; find the first User_msg = last user question *)
  let rec collect_assistant acc = function
    | [] -> (acc, None)
    | User_msg u :: _ -> (acc, Some u)
    | Assistant_text t :: rest -> collect_assistant (t :: acc) rest
    | _ :: rest -> collect_assistant acc rest
  in
  let (texts, user) = collect_assistant [] entries in
  match user with
  | Some u when texts <> [] -> Some (u, String.concat "\n" texts)
  | _ -> None

let index_previous_turn s =
  match s.daemon with
  | Some d ->
    (match d.Urme_claude.Process.session_id, extract_last_turn s.entries with
     | Some sid, Some (user_text, assistant_text) ->
       let port = s.config.chromadb_port in
       let project = Filename.basename s.project_dir in
       Lwt.async (fun () ->
         Lwt.catch (fun () ->
           let* chroma_ok = check_port port in
           let* ollama_ok = check_ollama s.config.ollama_url in
           if not chroma_ok || not ollama_ok then Lwt.return_unit
           else
             let* collection_id =
               Urme_search.Chromadb.ensure_interactions_collection ~port ~project in
             let ts = Printf.sprintf "%.0f" (Unix.gettimeofday ()) in
             let idx = Hashtbl.hash (sid ^ ts) land 0x7FFFFFFF in
             let assistant_summary =
               if String.length assistant_text > 500
               then String.sub assistant_text 0 500 ^ "..."
               else assistant_text in
             Urme_search.Chromadb.save_interaction ~port ~collection_id
               ~experience_id:sid ~interaction_index:idx
               ~user_text ~assistant_summary
               ~user_uuid:(Printf.sprintf "%s_%d" sid idx)
               ~timestamp:ts ()
         ) (fun _exn -> Lwt.return_unit))
     | _ -> ())
  | None -> ()

(* ---------- Send message + event reader ---------- *)

let send_message mvar =
  let* s = Lwt_mvar.take mvar in
  if s.input_text = "" || s.streaming then
    Lwt_mvar.put mvar s
  else begin
    let text = s.input_text in
    index_previous_turn s;
    let s = maybe_refresh_usage mvar s in
    let* () = Lwt_mvar.put mvar { s with
      input_text = ""; entries = User_msg text :: s.entries;
      streaming = true; status_extra = "Claude is thinking...";
      stream_text = "" } in
    redraw mvar;
    let* daemon = ensure_daemon mvar in
    let* () = Urme_claude.Process.send daemon ~text in
    Lwt.async (fun () ->
      let rec read_loop () =
        let* event = Urme_claude.Process.next_event daemon in
        match event with
        | None ->
          let* () = update mvar (fun s ->
            let entries = if s.stream_text <> "" then
              Assistant_text s.stream_text :: s.entries else s.entries in
            { s with streaming = false; daemon = None; entries;
                     stream_text = ""; status_extra = "Claude process ended." }) in
          redraw mvar; Lwt.return_unit

        | Some (Urme_claude.Stream.System_init { model; _ }) ->
          let* () = update mvar (fun s -> { s with status_model = model }) in
          redraw mvar; read_loop ()

        | Some (Urme_claude.Stream.Assistant_message { content; model; usage; _ }) ->
          let* () = update mvar (fun s ->
            let s = { s with
              status_model = model;
              context_tokens = usage.input_tokens;
              status_tokens_in = s.status_tokens_in + usage.input_tokens;
              status_tokens_out = s.status_tokens_out + usage.output_tokens } in
            let s = if s.stream_text <> "" then
              { s with entries = Assistant_text s.stream_text :: s.entries;
                       stream_text = "" }
            else s in
            List.fold_left (fun s block -> match block with
              | Urme_claude.Stream.Text t when t <> "" ->
                { s with entries = Assistant_text t :: s.entries }
              | Urme_claude.Stream.Thinking t when t <> "" ->
                { s with entries = Thinking_block t :: s.entries }
              | Urme_claude.Stream.Tool_use { id; name; input } ->
                let inline_diff = match name with
                  | "Edit" | "Write" | "Bash" -> Some (diff_of_tool_input name input)
                  | _ -> None in
                let s = { s with
                  tool_use_map = (id, name) :: s.tool_use_map;
                  entries = Tool_use_block { tool_name = name; tool_use_id = id;
                                            input; inline_diff;
                                            timestamp = Unix.gettimeofday () }
                            :: s.entries } in
                (match name with
                 | "Edit" | "Write" -> append_diff s (diff_of_tool_input name input)
                 | _ -> s)
              | Urme_claude.Stream.Tool_result { tool_use_id; content; is_error } ->
                let tn = match List.assoc_opt tool_use_id s.tool_use_map with
                  | Some n -> n | None -> "tool" in
                { s with entries = Tool_result_block { tool_name = tn; content;
                                                      is_error } :: s.entries }
              | _ -> s
            ) s content
          ) in
          redraw mvar; read_loop ()

        | Some (Urme_claude.Stream.Result { is_error; result; total_cost_usd;
                                            usage; _ }) ->
          let* () = update mvar (fun s ->
            let entries = if s.stream_text <> "" then
              Assistant_text s.stream_text :: s.entries else s.entries in
            let entries = if is_error then
              System_info ("Error: " ^ result) :: entries else entries in
            let ctx = usage.input_tokens + usage.output_tokens in
            { s with streaming = false; status_cost = total_cost_usd;
                     context_tokens = (if ctx > 0 then ctx else s.context_tokens);
                     status_tokens_in = usage.input_tokens;
                     status_tokens_out = usage.output_tokens;
                     stream_text = ""; entries = Turn_separator :: entries;
                     status_extra = "Ready" }) in
          redraw mvar; Lwt.return_unit

        | Some _ -> read_loop ()
      in
      read_loop ());
    Lwt.return_unit
  end

(* ---------- Main ---------- *)

let run ~config ~project_dir () =
  let project_dir = if project_dir = "." then Sys.getcwd ()
    else if Filename.is_relative project_dir
    then Filename.concat (Sys.getcwd ()) project_dir
    else project_dir in
  let mvar = Lwt_mvar.create (initial_state ~config ~project_dir) in
  let* term = Lazy.force LTerm.stdout in

  (* Permission server *)
  let* perm_server = Permission_server.start ~on_request:(fun req ->
    let waiter, resolver = Lwt.wait () in
    let* s = Lwt_mvar.take mvar in
    let diff_before = s.diff_text in
    let s = append_diff s (diff_of_tool_input req.Permission_server.tool_name
                             req.Permission_server.tool_input) in
    let* () = Lwt_mvar.put mvar { s with
      pending = Some {
        tool_name = req.tool_name; tool_input = req.tool_input;
        resolve = (fun allow -> Lwt.wakeup_later resolver allow) };
      status_extra = Printf.sprintf "Permission: %s [Y/N]"
        (summarize_tool_input req.tool_name req.tool_input) } in
    redraw mvar;
    let* allow = waiter in
    let* () = update mvar (fun s -> { s with
      pending = None;
      diff_text = (if allow then s.diff_text else diff_before);
      status_extra = "Claude is thinking..." }) in
    redraw mvar;
    Lwt.return allow
  ) () in
  let* () = update mvar (fun s -> { s with perm_server = Some perm_server }) in

  let* ui = LTerm_ui.create term (draw mvar) in
  let* () = update mvar (fun s -> { s with ui = Some ui }) in

  (* Initial usage fetch for Pro/Max plans *)
  (match config.plan with
   | Urme_core.Config.Pro | Urme_core.Config.Max -> refresh_usage mvar
   | Urme_core.Config.Api -> ());

  let handle_approval mvar allow =
    match (match peek mvar with Some s -> s.pending | None -> None) with
    | Some p -> p.resolve allow; redraw mvar; Lwt.return_unit
    | None -> Lwt.return_unit
  in

  let is_char c ch = Uchar.to_int c = Uchar.to_int (Uchar.of_char ch) in

  let do_quit () =
    let s = match peek mvar with Some s -> s | None -> initial_state ~config ~project_dir in
    (match s.daemon with Some d -> Urme_claude.Process.kill d | None -> ());
    (match s.perm_server with
     | Some ps ->
       Lwt.catch (fun () -> Permission_server.stop ps) (fun _ -> Lwt.return_unit)
       |> Lwt.ignore_result
     | None -> ());
    (* Gracefully stop services we started *)
    if s.started_chroma then
      ignore (Sys.command "pkill -TERM -f 'chroma run' 2>/dev/null");
    if s.started_ollama then
      ignore (Sys.command "pkill -TERM -x ollama 2>/dev/null");
    (* Quit UI, then hard-exit to skip at_exit handlers that hit stale FDs *)
    (try Lwt.ignore_result (LTerm_ui.quit ui) with _ -> ());
    Unix._exit 0
  in

  (* Navigate to result ri in the search_results list (works for both search and git links) *)
  let jump_to_result s ri =
    let h = s.history in
    match List.nth_opt h.search_results ri with
    | Some (session_id, user_text, _doc, _turn_idx, _ts, _dist) ->
      let jsonl_dir = Urme_search.Jsonl_reader.find_jsonl_dir
          ~project_dir:s.project_dir in
      let path = Filename.concat jsonl_dir (session_id ^ ".jsonl") in
      let new_si = let target = session_id ^ ".jsonl" in
        let rec find i = function
          | [] -> h.session_idx | p :: rest ->
            if Filename.basename p = target then i else find (i+1) rest
        in find 0 h.sessions in
      if not (Sys.file_exists path) then s
      else if h.return_mode = Git then
        (* Git link: interaction-aligned turns + turn_idx *)
        let turns = split_into_interaction_turns ~filepath:path in
        let ti = min (max 0 (_turn_idx - 1)) (max 0 (List.length turns - 1)) in
        let scroll_pos = match List.nth_opt turns ti,
          List.nth_opt s.git.link_candidates ri with
          | Some entries, Some link ->
            let ec = ref 0 in let pos = ref 0 in let found = ref false in
            List.iteri (fun i e -> if not !found then match e with
              | Tool_use_block { tool_name; _ }
                when tool_name = "Edit" || tool_name = "Write" ->
                if !ec = link.entry_idx then (pos := i; found := true); incr ec
              | _ -> ()) entries; !pos
          | _ -> 0 in
        { s with history = { h with result_idx = ri; session_idx = new_si;
                                     showing_results = false;
                                     turns; turn_idx = ti; hist_scroll = scroll_pos };
                 git = { s.git with link_idx = ri };
                 status_extra = Printf.sprintf "Link %d/%d"
                   (ri + 1) (List.length h.search_results) }
      else
        (* Search result: match by user text *)
        let turns = split_into_turns (load_session_entries path) in
        let needle = String.trim user_text in
        let ti = let rec f i = function
          | [] -> min _turn_idx (max 0 (List.length turns - 1))
          | turn :: rest ->
            if List.exists (fun e -> match e with
              | User_msg t -> let t = String.trim t in
                (needle <> "" && String.length t >= String.length needle &&
                 String.sub t 0 (String.length needle) = needle) || t = needle
              | _ -> false) turn then i else f (i+1) rest
          in f 0 turns in
        { s with history = { h with result_idx = ri; session_idx = new_si;
                                     showing_results = false;
                                     turns; turn_idx = ti; hist_scroll = 0 };
                 status_extra = Printf.sprintf "Result %d/%d"
                   (ri + 1) (List.length h.search_results) }
    | None -> s
  in

  let rec loop () =
    let* event = LTerm_ui.wait ui in
    match event with
    | LTerm_event.Key { code = LTerm_key.Char c; control = true; _ }
      when is_char c 'c' || is_char c 'd' ->
      do_quit ()

    | LTerm_event.Key { code = LTerm_key.Char c; control = true; _ }
      when is_char c 'v' ->
      let* () = update mvar (fun s -> { s with verbose = not s.verbose }) in
      redraw mvar; loop ()

    | LTerm_event.Key { code = LTerm_key.Char c; control = true; _ }
      when is_char c 'g' ->
      let* () = switch_to_git mvar in loop ()

    | LTerm_event.Key { code = LTerm_key.Char c; control = true; _ }
      when is_char c 'h' ->
      let* () = switch_to_history mvar in loop ()

    | LTerm_event.Key { code = LTerm_key.Char c; control = false; meta = false; _ }
      when (match peek mvar with Some s -> s.pending <> None | None -> false)
        && (is_char c 'y' || is_char c 'Y') ->
      let* () = handle_approval mvar true in loop ()

    | LTerm_event.Key { code = LTerm_key.Char c; control = false; meta = false; _ }
      when (match peek mvar with Some s -> s.pending <> None | None -> false)
        && (is_char c 'n' || is_char c 'N') ->
      let* () = handle_approval mvar false in loop ()

    | _ when (match peek mvar with Some s -> s.pending <> None | None -> false) ->
      loop ()

    (* --- Palette open: intercept keys --- *)
    | LTerm_event.Key { code = LTerm_key.Enter; _ }
      when (match peek mvar with Some s -> s.palette_open | None -> false) ->
      let cmd = match peek mvar with
        | Some s ->
          let candidates = filter_commands s.input_text in
          let n = List.length candidates in
          if n > 0 then (List.nth candidates (s.palette_selected mod n)).cmd
          else s.input_text
        | None -> "" in
      let* () = update mvar (fun s ->
        { s with input_text = ""; palette_open = false; palette_selected = 0 }) in
      if cmd = "/exit" || cmd = "/quit" then do_quit ()
      else begin
        let* _handled = handle_command mvar cmd in
        redraw mvar; loop ()
      end

    | LTerm_event.Key { code = LTerm_key.Escape; _ }
      when (match peek mvar with Some s -> s.palette_open | None -> false) ->
      let* () = update mvar (fun s ->
        { s with input_text = ""; palette_open = false; palette_selected = 0 }) in
      redraw mvar; loop ()

    | LTerm_event.Key { code = LTerm_key.Up; _ }
      when (match peek mvar with Some s -> s.palette_open | None -> false) ->
      let* () = update mvar (fun s ->
        let n = List.length (filter_commands s.input_text) in
        let sel = if n = 0 then 0
          else (s.palette_selected - 1 + n) mod n in
        { s with palette_selected = sel }) in
      redraw mvar; loop ()

    | LTerm_event.Key { code = LTerm_key.Down; _ }
      when (match peek mvar with Some s -> s.palette_open | None -> false) ->
      let* () = update mvar (fun s ->
        let n = List.length (filter_commands s.input_text) in
        let sel = if n = 0 then 0
          else (s.palette_selected + 1) mod n in
        { s with palette_selected = sel }) in
      redraw mvar; loop ()

    | LTerm_event.Key { code = LTerm_key.Backspace; _ }
      when (match peek mvar with Some s -> s.palette_open | None -> false) ->
      let* () = update mvar (fun s ->
        let len = String.length s.input_text in
        if len > 0 then
          let text = String.sub s.input_text 0 (len - 1) in
          { s with input_text = text; palette_selected = 0;
                   palette_open = String.length text > 0 && text.[0] = '/' }
        else
          { s with palette_open = false; palette_selected = 0 }) in
      redraw mvar; loop ()

    | LTerm_event.Key { code = LTerm_key.Char c; control = false; meta = false; _ }
      when (match peek mvar with Some s -> s.palette_open | None -> false) ->
      let ch = Uchar.to_int c in
      let* () =
        if ch >= 32 && ch < 127 then
          let* () = update mvar (fun s ->
            { s with input_text = s.input_text ^ String.make 1 (Char.chr ch);
                     palette_selected = 0 }) in
          (redraw mvar; Lwt.return_unit)
        else Lwt.return_unit
      in
      loop ()

    (* --- Git mode keys --- *)
    | LTerm_event.Key { code = LTerm_key.Escape; _ }
      when (match peek mvar with Some s -> s.mode = Git | None -> false) ->
      let* () = update mvar (fun s ->
        match s.git.file_diff_filter with
        | Some _ ->
          { s with git = { s.git with file_diff_filter = None; diff_scroll_git = 0 };
                   status_extra = "Git browser" }
        | None ->
          { s with mode = Conv; status_extra = "Ready" }) in
      redraw mvar; loop ()

    (* Tab: next panel *)
    | LTerm_event.Key { code = LTerm_key.Tab; shift = false; _ }
      when (match peek mvar with Some s -> s.mode = Git | None -> false) ->
      let* () = update mvar (fun s ->
        let g = s.git in
        let next = match g.focus with
          | Branches -> Commits | Commits -> Files
          | Files -> Links | Links -> Branches in
        let link_candidates = match next with
          | Links ->
            (match List.nth_opt g.commits g.commit_idx, List.nth_opt g.files g.file_idx with
             | Some (sha, _, _), Some file ->
               (match Hashtbl.find_opt g.git_links (sha, Filename.basename file) with
                | Some l -> l | None -> [])
             | _ -> [])
          | _ -> g.link_candidates in
        { s with git = { g with focus = next; link_candidates; link_idx = 0 } }) in
      redraw mvar; loop ()

    (* Shift+Tab: previous panel *)
    | LTerm_event.Key { code = LTerm_key.Tab; shift = true; _ }
      when (match peek mvar with Some s -> s.mode = Git | None -> false) ->
      let* () = update mvar (fun s ->
        let g = s.git in
        let prev = match g.focus with
          | Branches -> Links | Commits -> Branches
          | Files -> Commits | Links -> Files in
        let link_candidates = match prev with
          | Links ->
            (match List.nth_opt g.commits g.commit_idx, List.nth_opt g.files g.file_idx with
             | Some (sha, _, _), Some file ->
               (match Hashtbl.find_opt g.git_links (sha, Filename.basename file) with
                | Some l -> l | None -> [])
             | _ -> [])
          | _ -> g.link_candidates in
        { s with git = { g with focus = prev; link_candidates; link_idx = 0 } }) in
      redraw mvar; loop ()

    (* Up / k: navigate up *)
    | LTerm_event.Key { code = LTerm_key.Up; _ }
      when (match peek mvar with Some s -> s.mode = Git | None -> false) ->
      let* () = update mvar (fun s ->
        let g = s.git in
        let git = match g.focus with
          | Branches -> { g with branch_idx = max 0 (g.branch_idx - 1) }
          | Commits -> { g with commit_idx = prev_linked_commit g g.commit_idx }
          | Files -> { g with file_idx = prev_linked_file g g.file_idx }
          | Links -> { g with link_idx = max 0 (g.link_idx - 1) } in
        { s with git }) in
      redraw mvar; loop ()

    | LTerm_event.Key { code = LTerm_key.Char ch; _ }
      when Uchar.to_int ch = Char.code 'k'
        && (match peek mvar with Some s -> s.mode = Git | None -> false) ->
      let* () = update mvar (fun s ->
        let g = s.git in
        let git = match g.focus with
          | Branches -> { g with branch_idx = max 0 (g.branch_idx - 1) }
          | Commits -> { g with commit_idx = prev_linked_commit g g.commit_idx }
          | Files -> { g with file_idx = prev_linked_file g g.file_idx }
          | Links -> { g with link_idx = max 0 (g.link_idx - 1) } in
        { s with git }) in
      redraw mvar; loop ()

    (* Down / j: navigate down *)
    | LTerm_event.Key { code = LTerm_key.Down; _ }
      when (match peek mvar with Some s -> s.mode = Git | None -> false) ->
      let* () = update mvar (fun s ->
        let g = s.git in
        let git = match g.focus with
          | Branches -> { g with branch_idx = min (List.length g.branches - 1) (g.branch_idx + 1) }
          | Commits -> { g with commit_idx = next_linked_commit g g.commit_idx }
          | Files -> { g with file_idx = next_linked_file g g.file_idx }
          | Links -> { g with link_idx = min (max 0 (List.length g.link_candidates - 1)) (g.link_idx + 1) } in
        { s with git }) in
      redraw mvar; loop ()

    | LTerm_event.Key { code = LTerm_key.Char ch; _ }
      when Uchar.to_int ch = Char.code 'j'
        && (match peek mvar with Some s -> s.mode = Git | None -> false) ->
      let* () = update mvar (fun s ->
        let g = s.git in
        let git = match g.focus with
          | Branches -> { g with branch_idx = min (List.length g.branches - 1) (g.branch_idx + 1) }
          | Commits -> { g with commit_idx = next_linked_commit g g.commit_idx }
          | Files -> { g with file_idx = next_linked_file g g.file_idx }
          | Links -> { g with link_idx = min (max 0 (List.length g.link_candidates - 1)) (g.link_idx + 1) } in
        { s with git }) in
      redraw mvar; loop ()

    (* [ / ]: scroll diff up/down *)
    | LTerm_event.Key { code = LTerm_key.Char ch; _ }
      when Uchar.to_int ch = Char.code '['
        && (match peek mvar with Some s -> s.mode = Git | None -> false) ->
      let* () = update mvar (fun s ->
        { s with git = { s.git with diff_scroll_git = max 0 (s.git.diff_scroll_git - 1) } }) in
      redraw mvar; loop ()

    | LTerm_event.Key { code = LTerm_key.Char ch; _ }
      when Uchar.to_int ch = Char.code ']'
        && (match peek mvar with Some s -> s.mode = Git | None -> false) ->
      let* () = update mvar (fun s ->
        let g = s.git in
        let n = List.length (String.split_on_char '\n' g.diff_preview) in
        { s with git = { g with diff_scroll_git = min (max 0 (n - 1)) (g.diff_scroll_git + 1) } }) in
      redraw mvar; loop ()

    (* h: switch to history — if file has links, show them as a navigable list *)
    | LTerm_event.Key { code = LTerm_key.Char ch; control = false; _ }
      when Uchar.to_int ch = Char.code 'h'
        && (match peek mvar with Some s -> s.mode = Git | None -> false) ->
      let has_file_links = match peek mvar with
        | Some s ->
          let g = s.git in
          (match List.nth_opt g.commits g.commit_idx, List.nth_opt g.files g.file_idx with
           | Some (sha, _, _), Some file ->
             (match Hashtbl.find_opt g.git_links (sha, Filename.basename file) with
              | Some (_ :: _) -> true | _ -> false)
           | _ -> false)
        | None -> false in
      if has_file_links then begin
        let* () = update mvar (fun s ->
          let g = s.git in
          let links = match List.nth_opt g.commits g.commit_idx, List.nth_opt g.files g.file_idx with
            | Some (sha, _, _), Some file ->
              (match Hashtbl.find_opt g.git_links (sha, Filename.basename file) with
               | Some l -> l | None -> [])
            | _ -> [] in
          (* Convert links to search_results format *)
          let results = List.mapi (fun i (link : git_conv_link) ->
            let label = Printf.sprintf "t%d e%d %s"
              (link.turn_idx + 1) link.entry_idx link.edit_key in
            (link.session_id, label, "", link.turn_idx, "", Float.of_int i)
          ) links in
          let jsonl_dir = Urme_search.Jsonl_reader.find_jsonl_dir
              ~project_dir:s.project_dir in
          let sessions = Urme_search.Jsonl_reader.list_sessions ~jsonl_dir in
          { s with mode = History;
                   history = { sessions; session_idx = 0;
                               turns = []; turn_idx = 0; hist_scroll = 0;
                               return_mode = Git;
                               search_active = false; search_query = "";
                               search_results = results; result_idx = 0;
                               showing_results = true };
                   git = { g with link_candidates = links; link_idx = 0 };
                   status_extra = Printf.sprintf "%d links" (List.length links) }) in
        redraw mvar; loop ()
      end else begin
        let* () = switch_to_history mvar in loop ()
      end

    (* Enter: context-dependent activate *)
    | LTerm_event.Key { code = LTerm_key.Enter; _ }
      when (match peek mvar with Some s -> s.mode = Git | None ->
        let oc = open_out_gen [Open_append;Open_creat] 0o644 "/tmp/urme_key.log" in
        Printf.fprintf oc "Enter: peek=None (mvar taken?)\n%!"; close_out oc; false) ->
      let* s = Lwt_mvar.take mvar in
      let g = s.git in
      let* () = match g.focus with
        | Branches ->
          let branch = match List.nth_opt g.branches g.branch_idx with
            | Some b -> b | None -> g.current_branch in
          let* commits = Lwt.catch
            (fun () ->
              let* _out = Urme_git.Ops.run_git ~cwd:s.project_dir
                ["log"; branch; "--format=%H%n%at%n%s%n---"; "--max-count=200"] in
              let lines = String.split_on_char '\n' _out in
              let rec parse = function
                | sha :: ts :: msg :: "---" :: rest ->
                  let timestamp = try float_of_string ts with _ -> 0.0 in
                  (sha, timestamp, msg) :: parse rest
                | _ -> [] in
              Lwt.return (parse lines))
            (fun _ -> Lwt.return []) in
          let* diff_preview = match commits with
            | (sha, _, _) :: _ ->
              Lwt.catch (fun () -> Urme_git.Ops.commit_diff ~cwd:s.project_dir ~sha) (fun _ -> Lwt.return "")
            | [] -> Lwt.return "" in
          let* files = match commits with
            | (sha, _, _) :: _ ->
              Lwt.catch (fun () -> Urme_git.Ops.commit_changed_files ~cwd:s.project_dir ~sha) (fun _ -> Lwt.return [])
            | [] -> Lwt.return [] in
          let first_linked = next_linked_commit { g with commits; git_links = g.git_links } (-1) in
          let ci = if first_linked >= 0 then first_linked else 0 in
          Lwt_mvar.put mvar { s with git = { g with commits; commit_idx = ci;
                                                     files; file_idx = 0; diff_preview;
                                                     diff_scroll_git = 0; focus = Commits;
                                                     file_diff_filter = None } }
        | Commits ->
          (match List.nth_opt g.commits g.commit_idx with
           | Some (sha, _, _) ->
             let* diff = Lwt.catch
               (fun () -> Urme_git.Ops.commit_diff ~cwd:s.project_dir ~sha) (fun _ -> Lwt.return "") in
             let* files = Lwt.catch
               (fun () -> Urme_git.Ops.commit_changed_files ~cwd:s.project_dir ~sha) (fun _ -> Lwt.return []) in
             Lwt_mvar.put mvar { s with git = { g with diff_preview = diff; files;
                                                        file_idx = 0; diff_scroll_git = 0;
                                                        file_diff_filter = None } }
           | None -> Lwt_mvar.put mvar s)
        | Files ->
          (match List.nth_opt g.files g.file_idx with
           | Some file ->
             let file_base = Filename.basename file in
             (match g.file_diff_filter with
              | Some f when Filename.basename f = file_base ->
                let all_links = match List.nth_opt g.commits g.commit_idx with
                  | Some (sha, _, _) ->
                    (match Hashtbl.find_opt g.git_links (sha, file_base) with
                     | Some l -> l | None -> [])
                  | None -> [] in
                if all_links <> [] then
                  Lwt_mvar.put mvar { s with
                    git = { g with focus = Links; link_candidates = all_links;
                                   link_idx = 0 };
                    status_extra = Printf.sprintf "%d links for %s"
                      (List.length all_links) file_base }
                else
                  Lwt_mvar.put mvar { s with
                    git = { g with file_diff_filter = None; diff_scroll_git = 0 };
                    status_extra = Printf.sprintf "No links for %s" file_base }
              | _ ->
                Lwt_mvar.put mvar { s with
                  git = { g with file_diff_filter = Some file; diff_scroll_git = 0 };
                  status_extra = Printf.sprintf "Diff: %s" file })
           | None -> Lwt_mvar.put mvar s)
        | Links ->
          (match List.nth_opt g.link_candidates g.link_idx with
           | Some link ->
             let jsonl_dir = Urme_search.Jsonl_reader.find_jsonl_dir
                 ~project_dir:s.project_dir in
             let path = Filename.concat jsonl_dir (link.session_id ^ ".jsonl") in
             if Sys.file_exists path then begin
               (* Use interaction-aligned turns to match edit_extract's counting
                  (only String user messages start turns, not tool-result text) *)
               let turns = split_into_interaction_turns ~filepath:path in
               (* turn_idx is 1-based from edit_extract; turns list is 0-indexed *)
               (* turn_idx is 1-based from edit_extract; turns list is 0-indexed *)
               let ti = min (max 0 (link.turn_idx - 1)) (max 0 (List.length turns - 1)) in
               let new_session_idx =
                 let target = link.session_id ^ ".jsonl" in
                 let rec find i = function
                   | [] -> s.history.session_idx
                   | p :: rest ->
                     if Filename.basename p = target then i else find (i + 1) rest
                 in find 0 s.history.sessions in
               (* Find actual entry position: entry_idx counts only Edit/Write,
                  but hist_scroll indexes all entries in the turn *)
               let scroll_pos = match List.nth_opt turns ti with
                 | Some entries ->
                   let edit_count = ref 0 in
                   let pos = ref 0 in
                   let found = ref false in
                   List.iteri (fun i entry ->
                     if not !found then
                       match entry with
                       | Tool_use_block { tool_name; _ }
                         when tool_name = "Edit" || tool_name = "Write" ->
                         if !edit_count = link.entry_idx then begin
                           pos := i; found := true
                         end;
                         incr edit_count
                       | _ -> ()
                   ) entries;
                   !pos
                 | None -> 0 in
               Lwt_mvar.put mvar { s with
                 mode = History;
                 history = { s.history with
                   showing_results = false;
                   session_idx = new_session_idx;
                   turns; turn_idx = ti; hist_scroll = scroll_pos;
                   return_mode = Git };
                 status_extra = Printf.sprintf "Session: %s turn %d entry %d"
                   link.session_id (ti + 1) scroll_pos }
             end else
               Lwt_mvar.put mvar { s with
                 status_extra = Printf.sprintf "Session %s not found" link.session_id }
           | None -> Lwt_mvar.put mvar s)
      in
      redraw mvar; loop ()

    (* --- History mode: search input active --- *)
    | LTerm_event.Key { code = LTerm_key.Escape; _ }
      when (match peek mvar with
            | Some s -> s.mode = History && s.history.search_active | _ -> false) ->
      let* () = update mvar (fun s ->
        { s with history = { s.history with search_active = false; search_query = "" };
                 status_extra = "History" }) in
      redraw mvar; loop ()

    | LTerm_event.Key { code = LTerm_key.Enter; _ }
      when (match peek mvar with
            | Some s -> s.mode = History && s.history.search_active | _ -> false) ->
      let query = (match peek mvar with
        | Some s -> s.history.search_query | None -> "") in
      if query <> "" then begin
        let* () = execute_search mvar query in
        loop ()
      end else begin
        let* () = update mvar (fun s ->
          { s with history = { s.history with search_active = false } }) in
        redraw mvar; loop ()
      end

    | LTerm_event.Key { code = LTerm_key.Backspace; _ }
      when (match peek mvar with
            | Some s -> s.mode = History && s.history.search_active | _ -> false) ->
      let* () = update mvar (fun s ->
        let q = s.history.search_query in
        let q' = if String.length q > 0
          then String.sub q 0 (String.length q - 1) else "" in
        { s with history = { s.history with search_query = q' } }) in
      redraw mvar; loop ()

    | LTerm_event.Key { code = LTerm_key.Char c; control = false; _ }
      when (match peek mvar with
            | Some s -> s.mode = History && s.history.search_active | _ -> false) ->
      let ch = Uchar.to_int c in
      if ch >= 32 && ch < 127 then begin
        let* () = update mvar (fun s ->
          let q = s.history.search_query ^ String.make 1 (Char.chr ch) in
          { s with history = { s.history with search_query = q } }) in
        redraw mvar; loop ()
      end else loop ()

    (* --- History mode: showing search results --- *)
    | LTerm_event.Key { code = LTerm_key.Escape; _ }
      when (match peek mvar with
            | Some s -> s.mode = History && s.history.showing_results | _ -> false) ->
      let* () = update mvar (fun s ->
        if s.history.return_mode = Git then
          (* Back to Git mode *)
          { s with mode = Git; status_extra = "Git";
                   history = { s.history with showing_results = false; result_idx = 0;
                               search_results = [] } }
        else
          { s with history = { s.history with showing_results = false; result_idx = 0 };
                   status_extra = "History" }) in
      redraw mvar; loop ()

    | LTerm_event.Key { code = LTerm_key.Enter; _ }
      when (match peek mvar with
            | Some s -> s.mode = History && s.history.showing_results | _ -> false) ->
      let* () = update mvar (fun s ->
        jump_to_result s s.history.result_idx) in
      redraw mvar; loop ()

    | LTerm_event.Key { code = LTerm_key.Up; _ }
      when (match peek mvar with
            | Some s -> s.mode = History && s.history.showing_results | _ -> false) ->
      let* () = update mvar (fun s ->
        { s with history = { s.history with
            result_idx = max 0 (s.history.result_idx - 1) } }) in
      redraw mvar; loop ()

    | LTerm_event.Key { code = LTerm_key.Down; _ }
      when (match peek mvar with
            | Some s -> s.mode = History && s.history.showing_results | _ -> false) ->
      let* () = update mvar (fun s ->
        let max_idx = max 0 (List.length s.history.search_results - 1) in
        { s with history = { s.history with
            result_idx = min max_idx (s.history.result_idx + 1) } }) in
      redraw mvar; loop ()

    | LTerm_event.Key { code = LTerm_key.Char c; control = false; _ }
      when (match peek mvar with
            | Some s -> s.mode = History && s.history.showing_results | _ -> false)
        && (is_char c '/') ->
      let* () = update mvar (fun s ->
        { s with history = { s.history with search_active = true; search_query = "";
                                             showing_results = false } }) in
      redraw mvar; loop ()

    (* --- History mode: normal browsing --- *)
    | LTerm_event.Key { code = LTerm_key.Escape; _ }
      when (match peek mvar with Some s -> s.mode = History | None -> false) ->
      let* () = update mvar (fun s ->
        { s with mode = s.history.return_mode; status_extra = "Ready" }) in
      redraw mvar; loop ()

    | LTerm_event.Key { code = LTerm_key.Char c; control = false; _ }
      when (match peek mvar with Some s -> s.mode = History | None -> false)
        && (is_char c 'q') ->
      let* () = update mvar (fun s ->
        { s with mode = s.history.return_mode; status_extra = "Ready" }) in
      redraw mvar; loop ()

    | LTerm_event.Key { code = LTerm_key.Char c; control = false; _ }
      when (match peek mvar with Some s -> s.mode = History | None -> false)
        && (is_char c '/') ->
      let* () = update mvar (fun s ->
        { s with history = { s.history with search_active = true; search_query = "" } }) in
      redraw mvar; loop ()

    | LTerm_event.Key { code = LTerm_key.Char c; control = false; _ }
      when (match peek mvar with Some s -> s.mode = History | None -> false)
        && (is_char c 'i') ->
      let* () = index_sessions mvar in
      loop ()

    | LTerm_event.Key { code = LTerm_key.Char c; control = false; _ }
      when (match peek mvar with Some s -> s.mode = History | None -> false)
        && (is_char c 'w') ->
      (* Wipe ChromaDB collections and re-index *)
      let* () = update mvar (fun s ->
        { s with status_extra = "Wiping ChromaDB..." }) in
      redraw mvar;
      Lwt.async (fun () ->
        Lwt.catch (fun () ->
          let* s_val = Lwt_mvar.take mvar in
          let* () = Lwt_mvar.put mvar s_val in
          let port = s_val.config.chromadb_port in
          let project = Filename.basename s_val.project_dir in
          let* () = ensure_services mvar ~chromadb_port:port
              ~ollama_url:s_val.config.ollama_url ~project_dir:s_val.project_dir in
          let* () = Lwt.catch (fun () ->
            Urme_search.Chromadb.delete_collection ~port
              ~name:(project ^ "_interactions")) (fun _ -> Lwt.return_unit) in
          let* () = Lwt.catch (fun () ->
            Urme_search.Chromadb.delete_collection ~port
              ~name:(project ^ "_experiences")) (fun _ -> Lwt.return_unit) in
          let* () = update mvar (fun s ->
            { s with status_extra = "Wiped. Re-indexing..." }) in
          redraw mvar;
          (* Now run index_sessions *)
          index_sessions mvar
        ) (fun exn ->
          let* () = update mvar (fun s ->
            { s with status_extra = Printf.sprintf "Wipe error: %s"
                (Printexc.to_string exn) }) in
          redraw mvar; Lwt.return_unit));
      loop ()

    (* Shift+Left/Right: jump to prev/next result (search or git link) *)
    | LTerm_event.Key { code = LTerm_key.Left; shift = true; _ }
      when (match peek mvar with
            | Some s -> s.mode = History && s.history.search_results <> []
                        && not s.history.showing_results | _ -> false) ->
      let* () = update mvar (fun s ->
        let ri = max 0 (s.history.result_idx - 1) in
        if ri = s.history.result_idx then s
        else jump_to_result s ri) in
      redraw mvar; loop ()

    | LTerm_event.Key { code = LTerm_key.Right; shift = true; _ }
      when (match peek mvar with
            | Some s -> s.mode = History && s.history.search_results <> []
                        && not s.history.showing_results | _ -> false) ->
      let* () = update mvar (fun s ->
        let max_ri = max 0 (List.length s.history.search_results - 1) in
        let ri = min max_ri (s.history.result_idx + 1) in
        if ri = s.history.result_idx then s
        else jump_to_result s ri) in
      redraw mvar; loop ()

    (* b: back to results list *)
    | LTerm_event.Key { code = LTerm_key.Char c; control = false; _ }
      when (match peek mvar with
            | Some s -> s.mode = History && s.history.search_results <> []
                        && not s.history.showing_results | _ -> false)
        && (is_char c 'b') ->
      let* () = update mvar (fun s ->
        let label = if s.history.return_mode = Git then
          Printf.sprintf "%d links" (List.length s.history.search_results)
        else Printf.sprintf "Search: \"%s\"" s.history.search_query in
        { s with history = { s.history with showing_results = true };
                 status_extra = label }) in
      redraw mvar; loop ()

    | LTerm_event.Key { code = LTerm_key.Left; _ }
      when (match peek mvar with Some s -> s.mode = History | None -> false) ->
      let* () = update mvar (fun s ->
        let h = s.history in
        if h.turn_idx > 0 then
          (* Previous turn in same session *)
          { s with history = { h with turn_idx = h.turn_idx - 1; hist_scroll = 0 } }
        else
          (* Go to previous session, last turn *)
          let si = max 0 (h.session_idx - 1) in
          if si <> h.session_idx then
            let turns = match List.nth_opt h.sessions si with
              | Some path -> split_into_turns (load_session_entries path)
              | None -> [] in
            let ti = max 0 (List.length turns - 1) in
            { s with history = { h with session_idx = si; turns; turn_idx = ti;
                                         hist_scroll = 0 } }
          else s) in
      redraw mvar; loop ()

    | LTerm_event.Key { code = LTerm_key.Right; _ }
      when (match peek mvar with Some s -> s.mode = History | None -> false) ->
      let* () = update mvar (fun s ->
        let h = s.history in
        if h.turn_idx < List.length h.turns - 1 then
          (* Next turn in same session *)
          { s with history = { h with turn_idx = h.turn_idx + 1; hist_scroll = 0 } }
        else
          (* Go to next session, first turn *)
          let max_si = max 0 (List.length h.sessions - 1) in
          let si = min max_si (h.session_idx + 1) in
          if si <> h.session_idx then
            let turns = match List.nth_opt h.sessions si with
              | Some path -> split_into_turns (load_session_entries path)
              | None -> [] in
            { s with history = { h with session_idx = si; turns; turn_idx = 0;
                                         hist_scroll = 0 } }
          else s) in
      redraw mvar; loop ()

    | LTerm_event.Key { code = LTerm_key.Up; _ }
      when (match peek mvar with Some s -> s.mode = History | None -> false) ->
      let* () = update mvar (fun s ->
        { s with history = { s.history with
          hist_scroll = max 0 (s.history.hist_scroll - 1) } }) in
      redraw mvar; loop ()

    | LTerm_event.Key { code = LTerm_key.Down; _ }
      when (match peek mvar with Some s -> s.mode = History | None -> false) ->
      let* () = update mvar (fun s ->
        { s with history = { s.history with
          hist_scroll = s.history.hist_scroll + 1 } }) in
      redraw mvar; loop ()

    | _ when (match peek mvar with Some s -> s.mode = Git || s.mode = History | None -> false) ->
      loop ()

    (* --- Conv mode keys --- *)
    | LTerm_event.Key { code = LTerm_key.Enter; _ } ->
      let text = (match peek mvar with Some s -> s.input_text | None -> "") in
      if text = "/exit" || text = "/quit" then begin
        let* () = update mvar (fun s -> { s with input_text = "" }) in
        do_quit ()
      end else begin
        let is_cmd = String.length text > 0 && text.[0] = '/' in
        let* () =
          if is_cmd then begin
            let* () = update mvar (fun s -> { s with input_text = "" }) in
            let* _handled = handle_command mvar text in
            Lwt.return_unit
          end else
            send_message mvar
        in
        loop ()
      end

    | LTerm_event.Key { code = LTerm_key.Backspace; _ } ->
      let* () = update mvar (fun s ->
        let len = String.length s.input_text in
        if len > 0 then { s with input_text = String.sub s.input_text 0 (len - 1) }
        else s) in
      redraw mvar; loop ()

    | LTerm_event.Key { code = LTerm_key.Char c; control = false; meta = false; _ } ->
      let ch = Uchar.to_int c in
      let* () =
        if ch >= 32 && ch < 127 then
          let chr = Char.chr ch in
          let* () = update mvar (fun s ->
            let text = s.input_text ^ String.make 1 chr in
            let open_palette = s.input_text = "" && chr = '/' in
            { s with input_text = text;
                     palette_open = open_palette;
                     palette_selected = 0 }) in
          (redraw mvar; Lwt.return_unit)
        else Lwt.return_unit
      in
      loop ()

    | LTerm_event.Key { code = LTerm_key.Tab; _ } ->
      let* () = update mvar (fun s -> { s with active_pane =
        match s.active_pane with `Conv -> `Diff | `Diff -> `Conv }) in
      redraw mvar; loop ()

    | LTerm_event.Key { code = LTerm_key.Up; _ } ->
      let* () = update mvar (fun s -> match s.active_pane with
        | `Conv -> { s with scroll_offset = s.scroll_offset + 1 }
        | `Diff -> { s with diff_scroll = s.diff_scroll + 1 }) in
      redraw mvar; loop ()

    | LTerm_event.Key { code = LTerm_key.Down; _ } ->
      let* () = update mvar (fun s -> match s.active_pane with
        | `Conv -> { s with scroll_offset = max 0 (s.scroll_offset - 1) }
        | `Diff -> { s with diff_scroll = max 0 (s.diff_scroll - 1) }) in
      redraw mvar; loop ()

    | LTerm_event.Resize _ -> redraw mvar; loop ()
    | _ -> loop ()
  in
  loop ()

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

type ui_mode = Git | History


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
  human_diffs : (string * string, string) Hashtbl.t;
    (* (commit_sha, file_basename) → human-only diff text *)
  link_candidates : git_conv_link list;
}

(* View state for a single opened search result:
   - Overview: compact panel of step properties, no turn body.
   - Body: full user+assistant transcript of the turn (legacy view). *)
type result_view = Overview | Body

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
  result_view : result_view;        (* Overview vs Body when drilling into a result *)
  search_synthesis : string;        (* Claude's answer sentence from smart/deep search *)
  searching : bool;                 (* True while an async search is in flight *)
}

let empty_git_state = {
  branches = []; current_branch = ""; commits = []; files = [];
  focus = Branches; branch_idx = 0; commit_idx = 0; file_idx = 0;
  link_idx = 0; diff_preview = ""; diff_scroll_git = 0;
  file_diff_filter = None; git_links = Hashtbl.create 0;
  human_edits = Hashtbl.create 0; human_diffs = Hashtbl.create 0; link_candidates = [];
}

let empty_history_state = {
  sessions = []; session_idx = 0; turns = []; turn_idx = 0;
  hist_scroll = 0; return_mode = History;
  search_active = false; search_query = "";
  search_results = []; result_idx = 0; showing_results = false;
  result_view = Overview; search_synthesis = ""; searching = false;
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
  { cmd = "/exit";     description = "Exit urme" };
  { cmd = "/git";      description = "Open git browser" };
  { cmd = "/history";  description = "Browse past conversations" };
  { cmd = "/verbose";  description = "Toggle verbose mode" };
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
  input_text : string;
  config : Urme_core.Config.config;
  project_dir : string;
  status_extra : string;
  verbose : bool;
  palette_open : bool;
  palette_selected : int;
  git : git_state;
  history : history_state;
  usage : Urme_core.Usage.usage_data option;
  tui_bridge : Tui_bridge.t option;
}

(* ---------- Helpers ---------- *)


let initial_state ~config ~project_dir = {
  mode = History;
  input_text = "";
  config; project_dir;
  status_extra = "Ready";
  verbose = true;
  palette_open = false; palette_selected = 0;
  git = empty_git_state; history = empty_history_state;
  usage = None;
  tui_bridge = None;
}


(* ---------- Terminal ref + redraw (forward-declared, filled in after render_screen) ---------- *)

let term_ref : Notty_lwt.Term.t option ref = ref None

(* redraw is defined after render_screen; use a ref for forward reference *)
let redraw_ref : (state Lwt_mvar.t -> unit) ref = ref (fun _ -> ())
let redraw mvar = !redraw_ref mvar

(* ---------- Colors (from config theme) ---------- *)

let _theme = (Urme_core.Config.load ()).theme
let lc (c : Urme_core.Config.rgb) = Notty.A.rgb_888 ~r:c.r ~g:c.g ~b:c.b
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
        (* Replace control chars (including tabs) with space — Notty chokes *)
        if byte >= 0x20 then
          Buffer.add_char buf s.[i]
        else
          Buffer.add_char buf ' ';
        loop (i + 1)
      end else
        let expected =
          if byte land 0xE0 = 0xC0 then 2
          else if byte land 0xF0 = 0xE0 then 3
          else if byte land 0xF8 = 0xF0 then 4
          else 0 in
        if expected = 0 || i + expected > len then
          (* drop invalid lead byte *)
          loop (i + 1)
        else
          let rec valid j =
            j >= expected || (Char.code s.[i + j] land 0xC0 = 0x80 && valid (j + 1))
          in
          if valid 1 then begin
            Buffer.add_string buf (String.sub s i expected);
            loop (i + expected)
          end else
            (* drop invalid UTF-8 fragment *)
            loop (i + 1)
  in
  loop 0

(* Advance one UTF-8 code point from pos in s, return new pos *)
let utf8_next s pos =
  if pos >= String.length s then pos
  else
    let b = Char.code s.[pos] in
    let step =
      if b < 0x80 then 1
      else if b land 0xE0 = 0xC0 then 2
      else if b land 0xF0 = 0xE0 then 3
      else if b land 0xF8 = 0xF0 then 4
      else 1 in
    min (pos + step) (String.length s)

let wrap_text text width =
  if width <= 0 then [""]
  else
    String.split_on_char '\n' text
    |> List.concat_map (fun line ->
      let len = String.length line in
      if len <= width then [line]
      else
        (* Split into chunks of at most [width] code points each *)
        let rec split acc pos =
          if pos >= len then List.rev acc
          else
            let rec advance p n =
              if n <= 0 || p >= len then p
              else advance (utf8_next line p) (n - 1) in
            let end_pos = advance pos width in
            let chunk = String.sub line pos (end_pos - pos) in
            split (chunk :: acc) end_pos
        in
        split [] 0)


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
             let inline_diff = None in
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
   Each turn = one real user interaction (type=user, content=String -> assistant response).
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
             let inline_diff = None in
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

(* Strip control chars (except \t \n) and replace invalid UTF-8 *)
let clean_output s =
  let buf = Buffer.create (String.length s) in
  let len = String.length s in
  let i = ref 0 in
  while !i < len do
    let b = Char.code s.[!i] in
    if b = 0x09 || b = 0x0a then begin
      Buffer.add_char buf s.[!i]; incr i
    end else if b < 0x20 || b = 0x7f then begin
      (* Control char — skip *)
      incr i
    end else if b < 0x80 then begin
      Buffer.add_char buf s.[!i]; incr i
    end else begin
      (* UTF-8 continuation expected *)
      let expected =
        if b land 0xE0 = 0xC0 then 2
        else if b land 0xF0 = 0xE0 then 3
        else if b land 0xF8 = 0xF0 then 4
        else 0 in
      if expected = 0 || !i + expected > len then begin
        incr i  (* skip invalid lead byte *)
      end else begin
        let valid = ref true in
        for j = 1 to expected - 1 do
          if Char.code s.[!i + j] land 0xC0 <> 0x80 then valid := false
        done;
        if !valid then begin
          Buffer.add_string buf (String.sub s !i expected);
          i := !i + expected
        end else
          incr i
      end
    end
  done;
  Buffer.contents buf

let truncate_result content =
  let content = clean_output content in
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
  fg : Notty.A.color;
  bg : Notty.A.color option;
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
  | Tool_use_block { tool_name; input; _ } ->
    let open Yojson.Safe.Util in
    let summary = match tool_name with
      | "Bash" ->
        let cmd = try input |> member "command" |> to_string with _ -> "" in
        if String.length cmd > 80 then String.sub cmd 0 77 ^ "..." else cmd
      | "Edit" | "Write" | "Read" ->
        (try input |> member "file_path" |> to_string with _ -> "")
      | "Grep" | "Glob" ->
        (try input |> member "pattern" |> to_string with _ -> "")
      | _ -> "" in
    let header =
      if summary = "" then Printf.sprintf "  [%s]" tool_name
      else Printf.sprintf "  [%s] %s" tool_name summary in
    let hdr = List.map (mk c_tool_name) (wrap_text header width) in
    (* For Edit/Write, show inline old/new as colored lines *)
    let body = match tool_name with
      | "Edit" ->
        let old_s = try input |> member "old_string" |> to_string with _ -> "" in
        let new_s = try input |> member "new_string" |> to_string with _ -> "" in
        let lines s prefix bg fg =
          String.split_on_char '\n' s
          |> List.filter_map (fun l ->
            if l = "" then None
            else Some (mk ~bg:(Some bg) fg ("    " ^ prefix ^ l))) in
        let old_lines = lines old_s "- " c_diff_del_bg c_diff_del_fg in
        let new_lines = lines new_s "+ " c_diff_add_bg c_diff_add_fg in
        let truncate ls = if List.length ls > 15 then
          List.filteri (fun i _ -> i < 15) ls @
          [mk c_fg (Printf.sprintf "    ... (%d more)" (List.length ls - 15))]
          else ls in
        truncate old_lines @ truncate new_lines
      | "Write" ->
        let content = try input |> member "content" |> to_string with _ -> "" in
        let lines = String.split_on_char '\n' content in
        let n = List.length lines in
        let shown = List.filteri (fun i _ -> i < 15) lines in
        List.map (fun l ->
          mk ~bg:(Some c_diff_add_bg) c_diff_add_fg ("    + " ^ l)) shown @
        (if n > 15 then [mk c_fg (Printf.sprintf "    ... (%d more)" (n - 15))]
         else [])
      | _ -> [] in
    hdr @ body
  | Tool_result_block { content; is_error; _ } ->
    let color = if is_error then c_error else c_tool_result in
    List.map (fun l -> mk color ("    " ^ l))
      (wrap_text (truncate_result content) (width - 4)) @ [mk c_fg ""]
  | System_info text ->
    (* Unified-diff colouring: + green bg, - red bg, @@ hunk header,
       +++/--- file header normal. Matches Edit/Write tool_use display. *)
    let starts c s = String.length s > 0 && s.[0] = c in
    let starts2 a b s = String.length s > 1 && s.[0] = a && s.[1] = b in
    List.concat_map (fun line ->
      let fg, bg =
        if starts2 '+' '+' line || starts2 '-' '-' line then (c_fg, None)
        else if starts '+' line then (c_diff_add_fg, Some c_diff_add_bg)
        else if starts '-' line then (c_diff_del_fg, Some c_diff_del_bg)
        else if starts '@' line then (c_highlight, None)
        else (c_fg, None)
      in
      List.map (fun wrapped -> mk ~bg fg ("  " ^ wrapped))
        (wrap_text line (width - 2))
    ) (String.split_on_char '\n' text)
  | Turn_separator ->
    [mk c_separator ("  " ^ String.make (min 40 (width - 2)) '-'); mk c_fg ""]

(* ---------- Notty image builders ---------- *)

(* Local infix operators to avoid opening Notty.I (which shadows width, height, pad, empty, string) *)
let ( <|> ) = Notty.I.( <|> )
let ( <-> ) = Notty.I.( <-> )
let ( </> ) = Notty.I.( </> )

(* Build a single row image of exactly width w, with given attr *)
(* Count UTF-8 code points in a string *)
let utf8_count s =
  let len = String.length s in
  let rec loop i n =
    if i >= len then n
    else loop (utf8_next s i) (n + 1)
  in loop 0 0

(* Truncate to at most [n] UTF-8 code points *)
let utf8_truncate s n =
  let len = String.length s in
  let rec advance p k =
    if k <= 0 || p >= len then p
    else advance (utf8_next s p) (k - 1) in
  let end_pos = advance 0 n in
  String.sub s 0 end_pos

let mk_row ~attr w text =
  let text = sanitize_utf8 text in
  let cps = utf8_count text in
  if cps = 0 then
    Notty.I.char attr ' ' w 1
  else
    let display_cps = min cps w in
    let text = if display_cps < cps then utf8_truncate text display_cps else text in
    let pad_n = max 0 (w - display_cps) in
    let txt_img = (try Notty.I.string attr text
      with _ -> Notty.I.string attr (String.make display_cps '?')) in
    if pad_n > 0 then txt_img <|> Notty.I.char attr ' ' pad_n 1
    else txt_img

(* Build an empty background block *)
let bg_block w h =
  Notty.I.char Notty.A.(bg c_bg) ' ' w h

(* Render the status bar (1 row) *)
let render_status_bar state w =
  let sbg = Notty.A.(fg c_fg ++ bg c_status_bg) in
  let mode_tag = match state.mode with
    | Git -> "GIT" | History -> "HIST" in
  let left = Printf.sprintf " [%s] urme" mode_tag in
  let right_str = match state.config.plan, state.usage with
    | (Urme_core.Config.Pro | Urme_core.Config.Max), Some u ->
      Urme_core.Usage.format_status_bar u
    | _ -> ""
  in
  (* Build the bar: left portion (bold highlight), center (status_extra), right *)
  let left_attr = Notty.A.(fg c_highlight ++ bg c_status_bg ++ st bold) in
  let left_img = mk_row ~attr:left_attr (String.length left) left in
  let right_len = String.length right_str in
  let center_start = max (String.length left + 2)
      ((w - String.length state.status_extra) / 2) in
  let center_end = center_start + String.length state.status_extra in
  let right_start = max 0 (w - right_len - 1) in
  (* Build as a single row: pad left, center text, right text *)
  let gap1 = max 0 (center_start - String.length left) in
  let gap2 = max 0 (right_start - center_end) in
  let remaining = max 0 (w - right_start - right_len) in
  let open Notty.I in
  left_img
  <|> char sbg ' ' gap1 1
  <|> (try string sbg (sanitize_utf8 state.status_extra) with _ -> empty)
  <|> char sbg ' ' gap2 1
  <|> (if right_len > 0 then
         (try string sbg (sanitize_utf8 right_str) with _ -> empty)
       else empty)
  <|> char sbg ' ' remaining 1

(* Render the conversation pane *)
(* ---------- Git mode rendering ---------- *)

(* lazygit-style boxed panel with ┌─[N]─Title─…─┐ header, │ … │ side
   borders, and └──┘ footer. Focused panel gets a brighter border
   colour so the active section is obvious at a glance. *)
let render_git_panel ~idx ~height ~title ~focused ~items ~sel_idx ~width =
  if height <= 0 || width < 2 then Notty.I.empty
  else begin
    let border_fg = if focused then c_highlight else c_separator in
    let border_attr = Notty.A.(fg border_fg ++ bg c_bg) in
    let title_attr  = Notty.A.(fg c_highlight ++ bg c_bg ++ st bold) in
    let inner_w = max 0 (width - 2) in
    let inner_h = max 0 (height - 2) in
    (* UTF-8 box-drawing needs bytes-not-code-points, so we build the
       dash fill via a buffer rather than [String.make]. *)
    let dash_fill n =
      let b = Buffer.create (n * 3) in
      for _ = 1 to n do Buffer.add_string b "─" done;
      Buffer.contents b in
    (* Top border: ┌─[N]─Title─────┐ *)
    let label = Printf.sprintf "[%d]─%s" idx title in
    let label_cps = utf8_count label in
    let top =
      if inner_w <= label_cps then
        Notty.I.string border_attr "┌" <|>
        Notty.I.string title_attr
          (if label_cps > inner_w then utf8_truncate label inner_w else label) <|>
        Notty.I.string border_attr "┐"
      else
        let fill_n = inner_w - label_cps - 1 in
        Notty.I.string border_attr "┌─" <|>
        Notty.I.string title_attr label <|>
        Notty.I.string border_attr (dash_fill (max 0 fill_n)) <|>
        Notty.I.string border_attr "┐" in
    (* Content rows — same scrolling logic as before, just +2 cols of
       side-border wrapping each row. *)
    let scroll = max 0 (sel_idx - inner_h + 1) in
    let row_for i (lbl, is_current, is_dim) =
      let vi = i - scroll in
      if vi >= 0 && vi < inner_h then begin
        let selected = focused && i = sel_idx in
        let fg_c = if selected then c_highlight
                 else if is_dim then c_separator
                 else if is_current then c_user
                 else c_fg in
        let bg_c = if selected then c_status_bg else c_bg in
        let attr = Notty.A.(fg fg_c ++ bg bg_c
                            ++ (if selected then st bold else empty)) in
        let inner_row = mk_row ~attr inner_w (" " ^ lbl) in
        Some (Notty.I.string border_attr "│"
              <|> inner_row
              <|> Notty.I.string border_attr "│")
      end else None
    in
    let visible_rows = List.filter_map Fun.id (List.mapi row_for items) in
    let filler_row =
      Notty.I.string border_attr "│"
      <|> Notty.I.char Notty.A.(fg c_fg ++ bg c_bg) ' ' inner_w 1
      <|> Notty.I.string border_attr "│" in
    let content =
      let used = List.length visible_rows in
      let pad = max 0 (inner_h - used) in
      let fillers =
        if pad = 0 then []
        else List.init pad (fun _ -> filler_row) in
      Notty.I.vcat (visible_rows @ fillers) in
    let bot =
      Notty.I.string border_attr "└" <|>
      Notty.I.string border_attr (dash_fill inner_w) <|>
      Notty.I.string border_attr "┘" in
    top <-> content <-> bot
  end

(* Derive the files list for the currently-selected commit from the
   already-loaded [git_links] hashtable — recomputed every time
   anything reads it. No caching, no manual refresh, no staleness. *)
let current_commit_sha g =
  match List.nth_opt g.commits g.commit_idx with
  | Some (s, _, _) -> s
  | None -> ""

let current_files g =
  let sha = current_commit_sha g in
  if sha = "" then []
  else
    Hashtbl.fold (fun (s, fb) _ acc ->
      if s = sha && not (List.mem fb acc) then fb :: acc else acc
    ) g.git_links []
    |> List.sort String.compare

let current_links g =
  let sha = current_commit_sha g in
  let files = current_files g in
  match List.nth_opt files g.file_idx with
  | Some fb ->
    (match Hashtbl.find_opt g.git_links (sha, fb) with
     | Some l -> l | None -> [])
  | None -> []

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

let render_git_left_panels state w h =
  let g = state.git in
  let total_h = h in
  let width = w in
  let branch_items = List.map (fun b ->
    let prefix = if b = g.current_branch then "* " else "  " in
    (prefix ^ b, b = g.current_branch, false)
  ) g.branches in
  let commit_items = List.map (fun (sha, _ts, msg) ->
    let short_sha = if String.length sha >= 7 then String.sub sha 0 7 else sha in
    let w' = max 1 (width - 10) in
    let msg_trunc = if String.length msg > w' then String.sub msg 0 w' else msg in
    let has_links = Hashtbl.fold (fun (s, _) _ found ->
      found || s = sha) g.git_links false in
    (Printf.sprintf " %s %s" short_sha msg_trunc, false, not has_links)
  ) g.commits in
  let cur_sha = current_commit_sha g in
  let cur_files = current_files g in
  let file_items = List.map (fun fb ->
    let has_links = Hashtbl.mem g.git_links (cur_sha, fb) in
    let has_human = Hashtbl.mem g.human_edits (cur_sha, fb) in
    let is_human = not has_links || has_human in
    ("  " ^ fb, is_human, false)
  ) cur_files in
  let cur_links = current_links g in
  let sorted_links = List.sort (fun (a : git_conv_link) (b : git_conv_link) ->
    let c = Int.compare a.turn_idx b.turn_idx in
    if c <> 0 then c else Int.compare a.entry_idx b.entry_idx
  ) cur_links in
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
      (Printf.sprintf "%s%s t%d e%d" marker sid link.turn_idx link.entry_idx,
       is_human, false)
  ) sorted_links in
  let panels = [
    ("Branches", g.focus = Branches, branch_items, g.branch_idx);
    ("Commits", g.focus = Commits, commit_items, g.commit_idx);
    ("Files", g.focus = Files, file_items, g.file_idx);
    (Printf.sprintf "Links (%d)" (List.length cur_links),
     g.focus = Links, link_items, g.link_idx);
  ] in
  let n_panels = List.length panels in
  let focused_idx = match g.focus with
    | Branches -> 0 | Commits -> 1 | Files -> 2 | Links -> 3 in
  (* Focused panel gets ~60% of the column, the others split the rest. *)
  let focused_h = max 5 (total_h * 3 / 5) in
  let rest_h = max 0 (total_h - focused_h) in
  let other_h = if n_panels > 1 then max 3 (rest_h / (n_panels - 1)) else 0 in
  let heights = List.mapi (fun i _ ->
    if i = focused_idx then focused_h else other_h
  ) panels in
  let sum_h = List.fold_left (+) 0 heights in
  let heights = if sum_h <> total_h then
    let diff = total_h - sum_h in
    List.mapi (fun i h -> if i = focused_idx then h + diff else h) heights
  else heights in
  let panel_images = List.mapi (fun i (title, focused, items, sel_idx) ->
    let panel_h = List.nth heights i in
    render_git_panel ~idx:(i + 1) ~height:panel_h ~title ~focused
      ~items ~sel_idx ~width
  ) panels in
  Notty.I.vcat panel_images

let render_git_diff state w h =
  let g = state.git in
  let header = match g.file_diff_filter with
    | Some f -> Printf.sprintf " Diff: %s " f
    | None -> " Diff " in
  let title_attr = Notty.A.(fg c_highlight ++ bg c_status_bg ++ st bold) in
  let title_row = mk_row ~attr:title_attr w
    (Printf.sprintf "%s%s" header
       (String.make (max 0 (w - String.length header)) ' ')) in
  let content_h = max 0 (h - 1) in
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
        (line, Notty.A.cyan, c_bg)
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
  let scroll = g.diff_scroll_git in
  let visible = List.filteri (fun i _ -> i >= scroll && i < scroll + content_h) numbered in
  let rows = List.map (fun (text, fg_c, bg_c) ->
    let attr = Notty.A.(fg fg_c ++ bg bg_c) in
    mk_row ~attr w (" " ^ text)
  ) visible in
  let content_img = match rows with
    | [] -> bg_block w content_h
    | _ ->
      let img = Notty.I.vcat rows in
      let used = Notty.I.height img in
      if used < content_h then
        img <-> bg_block w (content_h - used)
      else img in
  title_row <-> content_img

(* ---------- History mode rendering ---------- *)

let render_history_results state w h =
  let hs = state.history in
  let title = Printf.sprintf " Search: \"%s\" (%d results)"
      hs.search_query (List.length hs.search_results) in
  let title_attr = Notty.A.(fg c_highlight ++ bg c_status_bg ++ st bold) in
  let title_row = mk_row ~attr:title_attr w
    (Printf.sprintf "%s%s" title (String.make (max 0 (w - String.length title)) ' ')) in
  let content_h = max 0 (h - 1) in
  (* If Claude produced a synthesis sentence, wrap it and render as a
     highlighted banner above the result list. *)
  let synthesis_rows, synthesis_h =
    if hs.search_synthesis = "" then ([], 0)
    else
      let margin = 4 in
      let maxw = max 10 (w - margin) in
      let wrap s =
        let rec go acc s =
          if String.length s <= maxw then List.rev (s :: acc)
          else
            let cut = ref maxw in
            while !cut > 0 && s.[!cut] <> ' ' do decr cut done;
            if !cut = 0 then cut := maxw;
            let head = String.sub s 0 !cut in
            let tail = String.sub s !cut (String.length s - !cut)
                       |> String.trim in
            go (head :: acc) tail
        in go [] s
      in
      let wrapped = wrap hs.search_synthesis in
      let attr = Notty.A.(fg c_assistant ++ bg c_bg ++ st bold) in
      let rows = List.mapi (fun i line ->
        let prefix = if i = 0 then "  ▍ " else "    " in
        mk_row ~attr w (prefix ^ line)
      ) wrapped in
      let sep = mk_row ~attr:Notty.A.(fg c_separator ++ bg c_bg) w
        ("  " ^ String.make (max 0 (w - 4)) '-') in
      (rows @ [sep], List.length rows + 1)
  in
  let content_h = max 0 (content_h - synthesis_h) in
  if hs.search_results = [] then
    let empty_msg = mk_row ~attr:Notty.A.(fg c_border ++ bg c_bg) w "  (no results)" in
    let fill = bg_block w (max 0 (content_h - 1)) in
    let base = title_row <-> empty_msg <-> fill in
    if synthesis_rows = [] then base
    else List.fold_left (fun acc r -> acc <-> r)
           (title_row) (synthesis_rows @ [empty_msg; fill])
  else begin
    let rows = ref [] in
    List.iteri (fun i (session_id, user_text, doc, turn_idx, _ts, distance) ->
      if List.length !rows < content_h then begin
        let selected = i = hs.result_idx in
        let marker = if selected then ">" else " " in
        let fg_c = if selected then c_highlight else c_fg in
        let bg_c = if selected then c_selection_bg else c_bg in
        (* Prefer the summary (doc) as the primary label — it's the
           distilled answer, not the raw prompt. Fall back to user_text
           only when the row wasn't summarised. *)
        let primary =
          if doc <> "" then doc
          else if user_text <> "" then user_text
          else "(no text)" in
        let label =
          let max_len = w - 14 in
          if String.length primary > max_len
          then String.sub primary 0 max_len ^ "..."
          else primary in
        let dist = Printf.sprintf "(%.2f)" distance in
        let line1 = Printf.sprintf " %s %s  %s" marker label dist in
        let line1 = if String.length line1 > w then String.sub line1 0 w else line1 in
        let attr1 = Notty.A.(fg fg_c ++ bg bg_c ++ (if selected then st bold else empty)) in
        rows := mk_row ~attr:attr1 w line1 :: !rows;
        if List.length !rows < content_h then begin
          let sid = if String.length session_id > 8
            then String.sub session_id 0 8 else session_id in
          let line2 = Printf.sprintf "   session: %s  step %d" sid turn_idx in
          let line2 = if String.length line2 > w then String.sub line2 0 w else line2 in
          (* Use c_tool_result (readable grey) for session line — c_border
             was so dim it looked unselectable. *)
          let attr2 = Notty.A.(fg c_tool_result ++ bg bg_c) in
          rows := mk_row ~attr:attr2 w line2 :: !rows
        end
      end
    ) hs.search_results;
    let content_img = match List.rev !rows with
      | [] -> bg_block w content_h
      | rs ->
        let img = Notty.I.vcat rs in
        let used = Notty.I.height img in
        if used < content_h then
          img <-> bg_block w (content_h - used)
        else img in
    let synthesis_block = match synthesis_rows with
      | [] -> Notty.I.empty
      | rs -> Notty.I.vcat rs in
    title_row <-> synthesis_block <-> content_img
  end

(* Compact property panel for a single turn — no body.

   Sources of the displayed data, in preference order:
   1. Current search result (if any search_results exist and we're in
      that flow). We already have tuple fields.
   2. Current History turn: pulled from SQLite using the open session's
      session_id and a best-effort turn-text match. *)

let fetch_step_meta_from_db ~project_dir ~session_id ~needle =
  try
    let db = Urme_store.Schema.open_or_create ~project_dir in
    let sql =
      "SELECT s.turn_index, s.timestamp, \
              COALESCE(s.summary,''), COALESCE(s.tags,''), \
              COALESCE(s.files_touched,'[]'), \
              COALESCE(s.commands_run,'[]'), \
              s.commit_before, s.commit_after, \
              COALESCE(s.thinking,'[]') \
       FROM steps s \
       WHERE s.session_id = ? AND s.prompt_text LIKE ? \
       LIMIT 1"
    in
    let needle_head =
      let n = String.trim needle in
      let n = if String.length n > 60 then String.sub n 0 60 else n in
      "%" ^ n ^ "%"
    in
    let rows = Urme_store.Db.query_list db sql
      [Sqlite3.Data.TEXT session_id; Sqlite3.Data.TEXT needle_head]
      ~f:(fun cols ->
        let s = Urme_store.Db.data_to_string in
        let so = Urme_store.Db.data_to_string_opt in
        (Urme_store.Db.data_to_int cols.(0),
         Urme_store.Db.data_to_float cols.(1),
         s cols.(2), s cols.(3), s cols.(4), s cols.(5),
         so cols.(6), so cols.(7), s cols.(8)))
    in
    Urme_store.Schema.close db;
    match rows with r :: _ -> Some r | [] -> None
  with _ -> None

let render_history_overview state w h =
  let hs = state.history in
  let content_h = max 0 (h - 1) in
  let sep = { fg = c_separator; bg = None; text = "" } in
  let field name value =
    { fg = c_tool_name; bg = None;
      text = Printf.sprintf "  %-14s %s" (name ^ ":") value } in
  let body_lines text color =
    let lines_split =
      String.split_on_char '\n' text
      |> List.concat_map (fun line ->
        if String.length line <= w - 4 then [line]
        else
          let rec chunks s =
            if String.length s <= w - 4 then [s]
            else String.sub s 0 (w - 4)
                 :: chunks (String.sub s (w - 4)
                              (String.length s - (w - 4)))
          in chunks line) in
    List.map (fun l -> { fg = color; bg = None; text = "    " ^ l })
      lines_split in

  let in_search = hs.search_results <> [] && not hs.showing_results in
  let header =
    if in_search then
      Printf.sprintf
        " Result %d/%d — Overview (Enter=body, ←/→=prev/next, b=list)"
        (hs.result_idx + 1) (List.length hs.search_results)
    else
      Printf.sprintf
        " Turn %d/%d — Overview (Enter=body, ←/→=prev/next turn)"
        (hs.turn_idx + 1) (List.length hs.turns)
  in
  let header = header ^ String.make (max 0 (w - String.length header)) ' ' in
  let title_attr = Notty.A.(fg c_highlight ++ bg c_status_bg ++ st bold) in
  let title_row = mk_row ~attr:title_attr w header in

  (* Extract per-turn "actions" (tool_uses) from the currently-loaded
     turn entries. Works for both search-result and plain-history views
     because jump_to_result loads hs.turns for the target session. *)
  let current_turn = List.nth_opt hs.turns hs.turn_idx in
  let actions_block =
    match current_turn with
    | None | Some [] -> []
    | Some entries ->
      let actions = List.filter_map (fun e ->
        match e with
        | Tool_use_block { tool_name; input; _ } ->
          let open Yojson.Safe.Util in
          let get_s k default =
            try input |> member k |> to_string with _ -> default in
          let summary = match tool_name with
            | "Bash" -> "$ " ^ get_s "command" ""
            | "Edit" | "Write" | "Read" -> get_s "file_path" ""
            | "Grep" -> "/" ^ get_s "pattern" "" ^ "/"
            | "Glob" -> get_s "pattern" ""
            | "Task" -> get_s "description" ""
            | "WebFetch" -> get_s "url" ""
            | "WebSearch" -> get_s "query" ""
            | _ ->
              (try
                 match input with
                 | `Assoc ((_, `String v) :: _) -> v
                 | _ -> ""
               with _ -> "")
          in
          let max_len = max 10 (w - 16) in
          let trimmed =
            if String.length summary > max_len
            then String.sub summary 0 max_len ^ "..."
            else summary
          in
          Some (tool_name, trimmed)
        | _ -> None
      ) entries in
      if actions = [] then []
      else
        [ sep;
          { fg = c_tool_name; bg = None;
            text = Printf.sprintf "  Actions (%d):" (List.length actions) };
        ]
        @ List.map (fun (name, summary) ->
            { fg = c_tool_name; bg = None;
              text = Printf.sprintf "    [%s] %s" name summary }
          ) actions
  in
  let lines =
    if in_search then begin
      match List.nth_opt hs.search_results hs.result_idx with
      | None -> [ { fg = c_border; bg = None; text = "  (no result)" } ]
      | Some (session_id, user_text, doc, turn_idx, ts_str, dist) ->
        let sid = if String.length session_id >= 8
          then String.sub session_id 0 8 else session_id in
        [ field "session" sid;
          field "turn" (string_of_int turn_idx);
          field "timestamp" ts_str;
          field "score" (Printf.sprintf "%.2f" dist);
          sep;
          { fg = c_assistant; bg = None; text = "  Summary:" };
        ]
        @ body_lines (if doc <> "" then doc else "(none)") c_assistant
        @ actions_block
        @ [ sep;
            { fg = c_user; bg = None; text = "  User message (excerpt):" };
          ]
        @ body_lines
            (let t = String.trim user_text in
             if t = "" then "(empty)"
             else if String.length t > 600
             then String.sub t 0 600 ^ "..."
             else t)
            c_user
    end else begin
      (* Main History mode: derive metadata from the open turn +
         best-effort DB lookup. *)
      let session_id = match List.nth_opt hs.sessions hs.session_idx with
        | Some path -> Filename.basename path |> Filename.chop_extension
        | None -> "" in
      let sid = if String.length session_id >= 8
        then String.sub session_id 0 8 else session_id in
      let current_turn = List.nth_opt hs.turns hs.turn_idx in
      let user_text = match current_turn with
        | Some entries ->
          (match List.find_opt (function User_msg _ -> true | _ -> false)
                  entries with
           | Some (User_msg t) -> t
           | _ -> "")
        | None -> ""
      in
      let meta = fetch_step_meta_from_db ~project_dir:state.project_dir
                   ~session_id ~needle:user_text in
      let common =
        [ field "session" sid; ] @
        (match meta with
         | Some (turn_index, ts, _, _, _, _, cb, ca, _) ->
           let tm = Unix.gmtime ts in
           let date = Printf.sprintf
             "%04d-%02d-%02d %02d:%02d"
             (tm.tm_year + 1900) (tm.tm_mon + 1) tm.tm_mday
             tm.tm_hour tm.tm_min in
           [ field "db turn" (string_of_int turn_index);
             field "timestamp" date;
             field "commit_before"
               (match cb with Some s ->
                  if String.length s >= 7 then String.sub s 0 7 else s
                | None -> "-");
             field "commit_after"
               (match ca with Some s ->
                  if String.length s >= 7 then String.sub s 0 7 else s
                | None -> "-"); ]
         | None ->
           [ field "turn" (string_of_int hs.turn_idx);
             { fg = c_border; bg = None;
               text = "  (no matching DB row for this turn)" }; ])
      in
      let summary_block = match meta with
        | Some (_, _, summary, tags, _, _, _, _, _) ->
          [ sep;
            { fg = c_assistant; bg = None; text = "  Summary:" } ]
          @ body_lines
              (if summary <> "" then summary else "(not yet summarised)")
              c_assistant
          @ (if tags <> ""
             then [ field "tags" tags ] else [])
        | None -> []
      in
      let thinking_block = match meta with
        | Some (_, _, _, _, _, _, _, _, thinking_json) ->
          let parse_arr j =
            try match Yojson.Safe.from_string j with
              | `List xs ->
                List.filter_map
                  (function `String s -> Some s | _ -> None) xs
              | _ -> []
            with _ -> []
          in
          let thoughts = parse_arr thinking_json in
          if thoughts = [] then []
          else
            [ sep;
              { fg = c_thinking; bg = None;
                text = Printf.sprintf "  Thinking (%d):"
                         (List.length thoughts) } ]
            @ List.concat_map (fun t ->
                let clipped =
                  let t = String.trim t in
                  if String.length t > 400
                  then String.sub t 0 400 ^ "..."
                  else t in
                body_lines clipped c_thinking) thoughts
        | None -> []
      in
      let files_cmds_block = match meta with
        | Some (_, _, _, _, files_json, cmds_json, _, _, _) ->
          let parse_arr j =
            try match Yojson.Safe.from_string j with
              | `List xs ->
                List.filter_map
                  (function `String s -> Some s | _ -> None) xs
              | _ -> []
            with _ -> []
          in
          let files = parse_arr files_json in
          let cmds = parse_arr cmds_json in
          (if files <> [] then
             [ sep;
               { fg = c_tool_name; bg = None;
                 text = Printf.sprintf "  Files (%d):" (List.length files) };
             ]
             @ List.map (fun f ->
                 { fg = c_fg; bg = None; text = "    " ^ f }) files
           else [])
          @ (if cmds <> [] then
               [ sep;
                 { fg = c_tool_name; bg = None;
                   text = Printf.sprintf "  Commands (%d):"
                            (List.length cmds) };
               ]
               @ List.map (fun c ->
                   { fg = c_fg; bg = None;
                     text = "    $ " ^
                            (if String.length c > w - 8
                             then String.sub c 0 (w - 11) ^ "..."
                             else c) }) cmds
             else [])
        | None -> []
      in
      let user_block =
        [ sep;
          { fg = c_user; bg = None; text = "  User message (excerpt):" } ]
        @ body_lines
            (let t = String.trim user_text in
             if t = "" then "(empty)"
             else if String.length t > 600
             then String.sub t 0 600 ^ "..."
             else t)
            c_user
      in
      common @ summary_block @ thinking_block @ files_cmds_block
      @ actions_block @ user_block
    end
  in
  let visible = List.filteri (fun i _ -> i < content_h) lines in
  let rows = List.map (fun l ->
    let attr = Notty.A.(fg l.fg ++ bg (match l.bg with Some b -> b | None -> c_bg)) in
    mk_row ~attr w l.text
  ) visible in
  let content_img = match rows with
    | [] -> bg_block w content_h
    | _ ->
      let img = Notty.I.vcat rows in
      let used = Notty.I.height img in
      if used < content_h then img <-> bg_block w (content_h - used)
      else img in
  title_row <-> content_img

let render_history_content state w h =
  let hs = state.history in
  if hs.searching then begin
    (* In-flight async search — replace the whole content with a
       prominent "Searching..." panel so the user doesn't see a stale
       step while Claude's NL→SQL + rerank + answer chain runs. *)
    let title = Printf.sprintf " Searching: \"%s\"" hs.search_query in
    let title = title ^ String.make (max 0 (w - String.length title)) ' ' in
    let title_attr = Notty.A.(fg c_highlight ++ bg c_status_bg ++ st bold) in
    let title_row = mk_row ~attr:title_attr w title in
    let content_h = max 0 (h - 1) in
    let msg1 = mk_row ~attr:Notty.A.(fg c_assistant ++ bg c_bg ++ st bold) w
      "  ⏳ Asking Claude to plan the search, rank candidates, and draft an answer…" in
    let msg2 = mk_row ~attr:Notty.A.(fg c_tool_result ++ bg c_bg) w
      "     (typically 5–15s)" in
    let fill = bg_block w (max 0 (content_h - 2)) in
    title_row <-> msg1 <-> msg2 <-> fill
  end
  else if hs.search_active then begin
    (* Search input prompt always wins — it's the active focus. *)
    let title = " Search history" in
    let title_attr = Notty.A.(fg c_highlight ++ bg c_status_bg ++ st bold) in
    let title_row = mk_row ~attr:title_attr w
      (Printf.sprintf "%s%s" title (String.make (max 0 (w - String.length title)) ' ')) in
    let content_h = max 0 (h - 1) in
    let prompt = Printf.sprintf " > %s_" hs.search_query in
    let prompt_attr = Notty.A.(fg c_highlight ++ bg c_bg) in
    let prompt_row = mk_row ~attr:prompt_attr w prompt in
    let gap = bg_block w 1 in
    let fill = bg_block w (max 0 (content_h - 2)) in
    title_row <-> gap <-> prompt_row <-> fill
  end
  else if hs.showing_results then
    render_history_results state w h
  else if hs.result_view = Overview then
    render_history_overview state w h
  else if false then begin  (* dead branch kept for structure, now in if-above *)
    (* Show search input *)
    let title = " Search history" in
    let title_attr = Notty.A.(fg c_highlight ++ bg c_status_bg ++ st bold) in
    let title_row = mk_row ~attr:title_attr w
      (Printf.sprintf "%s%s" title (String.make (max 0 (w - String.length title)) ' ')) in
    let content_h = max 0 (h - 1) in
    let prompt = Printf.sprintf " > %s_" hs.search_query in
    let prompt_attr = Notty.A.(fg c_highlight ++ bg c_bg) in
    let prompt_row = mk_row ~attr:prompt_attr w prompt in
    let gap = bg_block w 1 in  (* blank row before prompt *)
    let fill = bg_block w (max 0 (content_h - 2)) in
    title_row <-> gap <-> prompt_row <-> fill
  end else begin
    let n_turns = List.length hs.turns in
    let n_sessions = List.length hs.sessions in
    let title = if n_sessions = 0 then " History (no sessions)"
      else
        let sid = match List.nth_opt hs.sessions hs.session_idx with
          | Some path -> Filename.basename path |> Filename.chop_extension
          | None -> "?" in
        let short_id = if String.length sid > 8 then String.sub sid 0 8 else sid in
        let result_info = if hs.search_results <> [] && not hs.showing_results then
          Printf.sprintf "  [result %d/%d, b=list, S-arrows=jump]"
            (hs.result_idx + 1) (List.length hs.search_results)
        else "" in
        Printf.sprintf " Session %s  Turn %d/%d  (session %d/%d)%s"
          short_id (hs.turn_idx + 1) n_turns (hs.session_idx + 1) n_sessions result_info in
    let title_attr = Notty.A.(fg c_highlight ++ bg c_status_bg ++ st bold) in
    let title_row = mk_row ~attr:title_attr w
      (Printf.sprintf "%s%s" title (String.make (max 0 (w - String.length title)) ' ')) in
    let content_h = max 0 (h - 1) in
    let current_turn = List.nth_opt hs.turns hs.turn_idx in
    match current_turn with
    | None | Some [] ->
      let empty_msg = mk_row ~attr:Notty.A.(fg c_border ++ bg c_bg) w "  (no conversation loaded)" in
      let fill = bg_block w (max 0 (content_h - 1)) in
      (title_row <-> empty_msg <-> fill)
    | Some entries ->
      let entry_width = max 1 (w - 2) in
      let visible_entries = List.filteri (fun i _ -> i >= hs.hist_scroll) entries in
      let all_lines =
        List.concat_map (render_entry ~width:entry_width ~verbose:true) visible_entries in
      let visible = List.filteri (fun i _ -> i < content_h) all_lines in
      let rows = List.map (fun line ->
        let attr = Notty.A.(fg line.fg ++ bg (match line.bg with Some b -> b | None -> c_bg)) in
        mk_row ~attr w (" " ^ line.text)
      ) visible in
      let content_img = match rows with
        | [] -> bg_block w content_h
        | _ ->
          let img = Notty.I.vcat rows in
          let used = Notty.I.height img in
          if used < content_h then
            img <-> bg_block w (content_h - used)
          else img in
      title_row <-> content_img
  end

(* Render the input line (1 row) with colored key hints *)
(* Each hint is (key, description); rendered as key=highlight desc=dim sep=border *)
let render_hint_bar hints w =
  let key_attr = Notty.A.(fg c_highlight ++ bg c_input_bg ++ st bold) in
  let desc_attr = Notty.A.(fg c_fg ++ bg c_input_bg) in
  let sep_attr = Notty.A.(fg c_border ++ bg c_input_bg) in
  let pad_attr = Notty.A.(fg c_fg ++ bg c_input_bg) in
  let imgs = List.mapi (fun i (key, desc) ->
    let sep = if i > 0 then Notty.I.string sep_attr " | " else Notty.I.string pad_attr " " in
    let k = Notty.I.string key_attr key in
    let d = Notty.I.string desc_attr (" " ^ desc) in
    sep <|> k <|> d
  ) hints in
  let content = List.fold_left ( <|> ) Notty.I.empty imgs in
  let used = Notty.I.width content in
  let pad = max 0 (w - used) in
  content <|> Notty.I.char pad_attr ' ' pad 1

let render_input_line state w =
  let hints = match state.mode with
    | Git ->
      [("Tab", "panel"); ("j/k", "nav"); ("Enter", "select");
       ("[/]", "scroll"); ("h", "history"); ("Esc", "back"); ("q", "quit")]
    | History ->
      let h = state.history in
      if h.showing_results && h.return_mode = Git then
        [("\xe2\x86\x91\xe2\x86\x93", "select"); ("Enter", "view"); ("q", "quit")]
      else if h.showing_results then
        [("\xe2\x86\x91\xe2\x86\x93", "select"); ("Enter", "view"); ("q", "quit")]
      else if h.search_active then
        [("type query...", ""); ("Enter", "search"); ("Esc", "cancel")]
      else if h.return_mode = Git && h.search_results <> [] then
        [("\xe2\x86\x90/\xe2\x86\x92", "step"); ("S-arrows", "jump"); ("b", "list"); ("Esc", "git"); ("\xe2\x86\x91\xe2\x86\x93", "scroll")]
      else if h.search_results <> [] then
        [("\xe2\x86\x90/\xe2\x86\x92", "step"); ("S-arrows", "jump"); ("b", "list"); ("/", "search"); ("q", "quit")]
      else
        [("\xe2\x86\x90/\xe2\x86\x92", "step"); ("/", "search"); ("\xe2\x86\x91\xe2\x86\x93", "scroll"); ("g", "git"); ("i", "init"); ("u", "update"); ("q", "quit")]
  in
  render_hint_bar hints w

(* Render the command palette overlay *)
let render_palette state w h input_row =
  if not state.palette_open then Notty.I.void w h
  else begin
    let candidates = filter_commands state.input_text in
    let n = List.length candidates in
    if n = 0 then Notty.I.void w h
    else begin
      let max_show = min n 6 in
      let box_width = min (w - 2) 50 in
      let box_top = max 1 (input_row - max_show) in
      let sel_bg_c = Notty.A.rgb_888 ~r:50 ~g:50 ~b:90 in
      let norm_bg_c = Notty.A.rgb_888 ~r:35 ~g:35 ~b:60 in
      let sel_attr = Notty.A.(fg c_highlight ++ bg sel_bg_c ++ st bold) in
      let norm_attr = Notty.A.(fg c_fg ++ bg norm_bg_c) in
      let sel = state.palette_selected mod n in
      let scroll_off =
        if sel < max_show then 0
        else sel - max_show + 1 in
      let rows = List.mapi (fun i c ->
        let vi = i - scroll_off in
        if vi >= 0 && vi < max_show then begin
          let selected = i = sel in
          let attr = if selected then sel_attr else norm_attr in
          let label = Printf.sprintf " %-12s %s" c.cmd c.description in
          let label = if String.length label >= box_width
            then String.sub label 0 (box_width - 1) else label in
          let padded = label ^ String.make (max 0 (box_width - String.length label)) ' ' in
          Some (mk_row ~attr box_width padded)
        end else None
      ) candidates in
      let visible_rows = List.filter_map Fun.id rows in
      match visible_rows with
      | [] -> Notty.I.void w h
      | _ ->
        let palette_img = Notty.I.vcat visible_rows in
        (* Position: pad top by box_top rows, left by 1 col *)
        Notty.I.pad ~l:1 ~t:box_top palette_img
    end
  end

(* Main compose function: build the full screen image *)
let render_screen state w h =
  let status = render_status_bar state w in
  let input_row = h - 1 in
  let pane_height = max 1 (h - 2) in
  let content = match state.mode with
    | Git ->
      let left_w = max 1 (w / 4) in
      let right_w = max 1 (w - left_w - 1) in
      let left = render_git_left_panels state left_w pane_height in
      let sep_attr = Notty.A.(fg c_border ++ bg c_bg) in
      let sep = Notty.I.vcat (List.init pane_height (fun _ ->
        Notty.I.string sep_attr "\xe2\x94\x82")) in
      let right = render_git_diff state right_w pane_height in
      left <|> sep <|> right
    | History -> render_history_content state w pane_height in
  let input = render_input_line state w in
  let base = status <-> content <-> input in
  let palette = render_palette state w h input_row in
  base </> palette

(* Now wire up the real redraw *)
let () =
  redraw_ref := fun mvar ->
    match !term_ref, peek mvar with
    | Some term, Some state ->
      let (w, h) = Notty_lwt.Term.size term in
      let img = render_screen state w h in
      Lwt.async (fun () ->
        Lwt.catch (fun () ->
          let open Lwt.Syntax in
          let* () = Notty_lwt.Term.image term img in
          Notty_lwt.Term.cursor term None
        ) (fun _ -> Lwt.return_unit))
    | _ -> ()

(* ---------- Commit-centric git <-> conversation link index ---------- *)

let log_debug msg =
  let oc = open_out_gen [Open_append; Open_creat] 0o644 "/tmp/urme_debug.log" in
  Printf.fprintf oc "[%s] %s\n" (string_of_float (Unix.gettimeofday ())) msg;
  close_out oc

let parse_git_info = Urme_engine.Git_link_types.parse_git_info_json

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
               human_edits = Hashtbl.create 0; human_diffs = Hashtbl.create 0;
               link_candidates = [] }, debug)

(* Rebuild in-memory git_links from the [edit_links] table.

   Suppresses human-origin rows when a Claude row exists for the same
   (commit_sha, file_base): the walker walks each branch independently,
   and for commits shared across branches it can produce a spurious
   "human" row on the branches where the Claude edits aren't tagged
   (filtered out by the branch filter in walk_branch). Those are
   artefacts of the branch-walk, not real human edits. *)
let load_git_links_from_db ~project_dir =
  Lwt.catch (fun () ->
    let db = Urme_store.Schema.open_or_create ~project_dir in
    let all = Urme_store.Edit_links.all_with_commit db in
    (* First pass: collect (commit_sha, file_base) pairs that have at
       least one claude-origin row. *)
    let has_claude = Hashtbl.create 128 in
    List.iter (fun (row : Urme_store.Edit_links.row) ->
      if row.origin = "claude" then
        match row.commit_sha with
        | Some sha -> Hashtbl.replace has_claude (sha, row.file_base) ()
        | None -> ()
    ) all;
    let links : (string * string, git_conv_link list) Hashtbl.t =
      Hashtbl.create 256 in
    List.iter (fun (row : Urme_store.Edit_links.row) ->
      match row.commit_sha with
      | None -> ()
      | Some commit_sha ->
        let is_shadowed_human =
          row.origin = "human"
          && Hashtbl.mem has_claude (commit_sha, row.file_base)
        in
        if not is_shadowed_human then begin
          let session_id = match row.session_id with
            | Some s -> s
            | None -> "human"
          in
          let lk = (commit_sha, row.file_base) in
          let existing = match Hashtbl.find_opt links lk with
            | Some l -> l | None -> [] in
          if not (List.exists (fun (l : git_conv_link) ->
                    l.edit_key = row.edit_key) existing) then begin
            let link : git_conv_link = {
              commit_sha;
              file = row.file_base;
              session_id;
              turn_idx = row.turn_idx;
              entry_idx = row.entry_idx;
              edit_key = row.edit_key } in
            Hashtbl.replace links lk (existing @ [link])
          end
        end
    ) all;
    Urme_store.Schema.close db;
    Lwt.return links
  ) (fun _ -> Lwt.return (Hashtbl.create 0))

(* Pull (old, new) content for every human edit_link row, render a
   unified diff, and populate [human_diffs]. Synchronous — runs off
   the DB we already have open. *)
let load_human_diffs_from_db ~project_dir ~git_links =
  let diffs : (string * string, string) Hashtbl.t = Hashtbl.create 128 in
  (try
    let db = Urme_store.Schema.open_or_create ~project_dir in
    List.iter (fun (row : Urme_store.Edit_links.row) ->
      match row.commit_sha with
      | None -> ()
      | Some sha when row.origin = "human" ->
        (* Render a simple unified diff from the persisted content pair. *)
        let diff =
          match Patch.diff
                  (Some (row.file_base, row.old_content))
                  (Some (row.file_base, row.new_content)) with
          | Some p ->
            let buf = Buffer.create 512 in
            Patch.pp (Format.formatter_of_buffer buf) p;
            Buffer.contents buf
          | None ->
            if row.new_content = "" then row.old_content
            else row.new_content
        in
        Hashtbl.replace diffs (sha, row.file_base) diff
      | _ -> ()
    ) (Urme_store.Edit_links.all_with_commit db);
    Urme_store.Schema.close db
  with _ -> ());
  ignore git_links;
  diffs

let switch_to_git mvar =
  let* s = Lwt_mvar.take mvar in
  let* () = Lwt_mvar.put mvar { s with status_extra = "Loading git data..." } in
  redraw mvar;
  let* s = Lwt_mvar.take mvar in
  let* (git, debug) = load_git_data ~cwd:s.project_dir in
  let* git_links =
    if Hashtbl.length s.git.git_links > 0 then Lwt.return s.git.git_links
    else load_git_links_from_db ~project_dir:s.project_dir
  in
  let human_diffs =
    if Hashtbl.length s.git.human_diffs > 0 then s.git.human_diffs
    else load_human_diffs_from_db ~project_dir:s.project_dir ~git_links
  in
  let git = { git with git_links;
              human_edits = s.git.human_edits;
              human_diffs } in
  let status = if git.branches = [] then
    Printf.sprintf "Git: no branches! %s" debug
  else Printf.sprintf "Git browser (%d link keys)" (Hashtbl.length git_links) in
  let* () = Lwt_mvar.put mvar { s with mode = Git; git; status_extra = status } in
  redraw mvar; Lwt.return_unit

(* Extract individual turns (user_text, assistant_text) from a session JSONL *)
(* Index all session JSONL files into the V2 SQLite store, then run the
   per-edit git linker to populate edit_links. The Claude summarisation
   pass is kicked off separately from the `urme init` subcommand. *)
let index_sessions mvar =
  let* () = update mvar (fun s ->
    { s with status_extra = "Indexing into SQLite..." }) in
  redraw mvar;
  Lwt.async (fun () ->
    Lwt.catch (fun () ->
      let* s_val = Lwt_mvar.take mvar in
      let* () = Lwt_mvar.put mvar s_val in
      let cwd = s_val.project_dir in
      let db = Urme_store.Schema.open_or_create ~project_dir:cwd in
      let n = Urme_engine.Indexer.index_all_sessions ~db ~project_dir:cwd in
      let* () = update mvar (fun s ->
        { s with status_extra =
            Printf.sprintf "Indexed %d turns; building git links..." n }) in
      redraw mvar;
      let* () = Lwt.catch (fun () ->
        Urme_engine.Git_index.run_once ~project_dir:cwd ~db
      ) (fun _ -> Lwt.return_unit) in
      Urme_store.Schema.close db;
      let* git_links = load_git_links_from_db ~project_dir:cwd in
      let n_links = Hashtbl.length git_links in
      let* () = update mvar (fun s ->
        { s with git = { s.git with git_links };
                 status_extra =
            Printf.sprintf "Indexed %d turns | %d git links" n n_links }) in
      redraw mvar; Lwt.return_unit
    ) (fun exn ->
      let* () = update mvar (fun s ->
        { s with status_extra = Printf.sprintf "Index error: %s"
            (Printexc.to_string exn) }) in
      redraw mvar; Lwt.return_unit));
  Lwt.return_unit

let hit_to_tuple (h : Urme_search.Search.hit) =
  let tm = Unix.gmtime h.timestamp in
  let ts_str = Printf.sprintf
    "%04d-%02d-%02dT%02d:%02d:%02d.000Z"
    (tm.tm_year + 1900) (tm.tm_mon + 1) tm.tm_mday
    tm.tm_hour tm.tm_min tm.tm_sec in
  let session_id = Option.value h.session_id ~default:"" in
  let doc =
    if h.summary <> "" then h.summary
    else if h.prompt_text <> "" then h.prompt_text
    else "" in
  (session_id, h.prompt_text, doc, h.turn_index, ts_str, -. h.score)

(* Single search mode: Deep (NL→SQL + rerank + grounded answer). The
   earlier fast/smart variants are kept in the library for other
   callers but the TUI only exposes deep. *)
let execute_search mvar query =
  let* () = update mvar (fun s ->
    { s with status_extra = "Searching...";
             history = { s.history with
                         search_active = false;
                         searching = true;
                         search_synthesis = "";
                         search_results = [];
                         showing_results = false } }) in
  redraw mvar;
  Lwt.async (fun () ->
    Lwt.catch (fun () ->
      let* s_val = Lwt_mvar.take mvar in
      let* () = Lwt_mvar.put mvar s_val in
      let db = Urme_store.Schema.open_or_create ~project_dir:s_val.project_dir in
      let config = s_val.config in
      let* hits, synthesis =
        Urme_search.Search.run_deep
          ~db ~binary:config.claude_binary
          ~project_dir:s_val.project_dir ~limit:20 query
      in
      Urme_store.Schema.close db;
      let results = List.map hit_to_tuple hits in
      let status = Printf.sprintf "Found %d results" (List.length results) in
      let* () = update mvar (fun s ->
        { s with history = { s.history with
            search_results = results;
            showing_results = true;
            search_query = query;
            result_idx = 0;
            search_synthesis = synthesis;
            searching = false };
          status_extra = status }) in
      redraw mvar; Lwt.return_unit
    ) (fun exn ->
      let* () = update mvar (fun s ->
        { s with history = { s.history with
                              showing_results = false;
                              searching = false };
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
                         showing_results = false;
                         result_view = Overview;
                         search_synthesis = "";
                         searching = false };
             status_extra = Printf.sprintf "History (%d sessions)" n }) in
  redraw mvar; Lwt.return_unit

(* ---------- Background resync (indexer + summariser + linker) ----------

   Runs once at TUI startup, then every hour while the TUI is alive.
   Idempotent — indexer upserts by (session_id, turn_index), summariser
   only touches rows with NULL summary, linker walks the small diff from
   last-saved state. Concurrent runs are guarded by [refresh_busy]. *)

let refresh_busy = ref false

(* Background resync = indexer + incremental summariser + git linker.
   [Summarise.summarise_pending] only processes rows where summary
   IS NULL, so this is a no-op on steady state and costs a few Claude
   calls only when new turns appear. *)
let full_resync mvar =
  if !refresh_busy then Lwt.return_unit
  else begin
    refresh_busy := true;
    Lwt.finalize (fun () ->
      Lwt.catch (fun () ->
        let* s_val = Lwt_mvar.take mvar in
        let* () = Lwt_mvar.put mvar s_val in
        let cwd = s_val.project_dir in
        let config = s_val.config in
        let* () = update mvar (fun s ->
          { s with status_extra = "Background resync: indexing..." }) in
        redraw mvar;
        let db = Urme_store.Schema.open_or_create ~project_dir:cwd in
        let n = Urme_engine.Indexer.index_all_sessions ~db ~project_dir:cwd in
        let* () = update mvar (fun s ->
          { s with status_extra =
              Printf.sprintf "Resync: %d turns; summarising new..." n }) in
        redraw mvar;
        let* () =
          Lwt.catch (fun () ->
            Urme_engine.Summarise.summarise_pending
              ~binary:config.claude_binary ~db ())
            (fun _ -> Lwt.return_unit)
        in
        let* () = update mvar (fun s ->
          { s with status_extra = "Resync: building git links..." }) in
        redraw mvar;
        let* () = Lwt.catch (fun () ->
          Urme_engine.Git_index.run_once ~project_dir:cwd ~db)
          (fun _ -> Lwt.return_unit) in
        Urme_store.Schema.close db;
        let* () = update mvar (fun s ->
          { s with status_extra = "Ready" }) in
        redraw mvar; Lwt.return_unit
      ) (fun exn ->
        let* () = update mvar (fun s ->
          { s with status_extra =
              Printf.sprintf "Resync error: %s" (Printexc.to_string exn) }) in
        redraw mvar; Lwt.return_unit))
      (fun () -> refresh_busy := false; Lwt.return_unit)
  end

(* Hourly background loop — kicked off once at startup and runs for the
   life of the TUI. *)
let spawn_hourly_resync mvar =
  Lwt.async (fun () ->
    let rec loop () =
      let* () = Lwt_unix.sleep 3600.0 in
      let* () = full_resync mvar in
      loop ()
    in
    loop ())

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

(* ---------- Main ---------- *)

let run ~config ~project_dir () =
  let project_dir = if project_dir = "." then Sys.getcwd ()
    else if Filename.is_relative project_dir
    then Filename.concat (Sys.getcwd ()) project_dir
    else project_dir in
  let mvar = Lwt_mvar.create (initial_state ~config ~project_dir) in
  let term = Notty_lwt.Term.create ~mouse:false ~nosig:true () in
  term_ref := Some term;

  (* Load sessions for History mode on startup *)
  let* () = switch_to_history mvar in

  (* Initial render *)
  redraw mvar;

  (* Initial usage fetch for Pro/Max plans *)
  (match config.plan with
   | Urme_core.Config.Pro | Urme_core.Config.Max -> refresh_usage mvar
   | Urme_core.Config.Api -> ());

  (* One-shot resync on startup + hourly background loop. *)
  Lwt.async (fun () -> full_resync mvar);
  spawn_hourly_resync mvar;

  let do_quit () =
    let s = match peek mvar with Some s -> s | None -> initial_state ~config ~project_dir in
    (match s.tui_bridge with
     | Some tb ->
       Lwt.catch (fun () -> Tui_bridge.stop tb) (fun _ -> Lwt.return_unit)
       |> Lwt.ignore_result
     | None -> ());
    (* Quit terminal, then hard-exit to skip at_exit handlers that hit stale FDs *)
    (try Lwt.ignore_result (Notty_lwt.Term.release term) with _ -> ());
    Unix._exit 0
  in

  (* Navigate to result ri in the search_results list (works for both search and git links) *)
  (* Internal helper; wrapped below to force result_view = Overview on entry. *)
  let jump_to_result_raw s ri =
    let h = s.history in
    match List.nth_opt h.search_results ri with
    | Some (session_id, user_text, _doc, _turn_idx, _ts, _dist) ->
      (* Check if this is a human edit link. The walker uses a `human:`
         PREFIX on edit_keys (and session_id = "human"); V1 used a
         suffix — accept either shape. *)
      let is_human_link = match List.nth_opt s.git.link_candidates ri with
        | Some link ->
          let ek = link.edit_key in
          let n = String.length ek in
          (n >= 6 && String.sub ek 0 6 = "human:") ||
          (n > 6 && String.sub ek (n - 6) 6 = ":human") ||
          (n > 11 && String.sub ek (n - 11) 11 = ":human-only") ||
          link.session_id = "human"
        | None -> false in
      if is_human_link then
        let link_opt = List.nth_opt s.git.link_candidates ri in
        let file = match link_opt with Some l -> l.file | None -> "" in
        let commit_sha = match link_opt with Some l -> l.commit_sha | None -> "" in
        let diff_text = match Hashtbl.find_opt s.git.human_diffs (commit_sha, file) with
          | Some d -> d
          | None -> "No human-specific diff computed" in
        let short = if String.length commit_sha > 7
          then String.sub commit_sha 0 7 else commit_sha in
        if session_id = "" || session_id = "human" then
          (* Purely human — show diff only *)
          { s with history = { h with showing_results = false; result_idx = ri;
                                       turns = [[System_info (Printf.sprintf
                                         "Human edit to %s in %s\n\n%s" file short diff_text)]];
                                       turn_idx = 0; hist_scroll = 0 };
                   status_extra = Printf.sprintf "Human edit: %s" file }
        else
          (* Human modification of Claude edit — show Claude's turn + diff *)
          let jsonl_dir = Urme_search.Jsonl_reader.find_jsonl_dir
              ~project_dir:s.project_dir in
          let path = Filename.concat jsonl_dir (session_id ^ ".jsonl") in
          if Sys.file_exists path then
            let turns = split_into_interaction_turns ~filepath:path in
            let ti = min (max 0 (_turn_idx - 1)) (max 0 (List.length turns - 1)) in
            let note = System_info (Printf.sprintf
              "--- Human modified this Claude edit ---\n%s" diff_text) in
            let turns = List.mapi (fun i t ->
              if i = ti then t @ [note] else t) turns in
            let new_si = let target = session_id ^ ".jsonl" in
              let rec find i = function
                | [] -> h.session_idx | p :: rest ->
                  if Filename.basename p = target then i else find (i+1) rest
              in find 0 h.sessions in
            { s with history = { h with result_idx = ri; session_idx = new_si;
                                         showing_results = false;
                                         turns; turn_idx = ti; hist_scroll = 0 };
                     git = { s.git with link_idx = ri };
                     status_extra = Printf.sprintf "Human edit: %s t%d" file _turn_idx }
          else s
      else
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
        (* Search result: match by user text. The indexer preserves the
           raw prompt_text including XML wrappers like
           <local-command-stdout>…</…>, but the TUI's turn splitter
           strips those. So normalize both sides before comparing. *)
        let turns = split_into_turns (load_session_entries path) in
        let norm s = String.trim (strip_xml_tags s) in
        let needle = norm user_text in
        let needle_head =
          if String.length needle > 80
          then String.sub needle 0 80 else needle in
        let ti = let rec f i = function
          | [] -> min _turn_idx (max 0 (List.length turns - 1))
          | turn :: rest ->
            if List.exists (fun e -> match e with
              | User_msg t ->
                let t = norm t in
                needle_head <> ""
                && String.length t >= String.length needle_head
                && String.sub t 0 (String.length needle_head) = needle_head
              | _ -> false) turn then i else f (i+1) rest
          in f 0 turns in
        { s with history = { h with result_idx = ri; session_idx = new_si;
                                     showing_results = false;
                                     turns; turn_idx = ti; hist_scroll = 0 };
                 status_extra = Printf.sprintf "Result %d/%d"
                   (ri + 1) (List.length h.search_results) }
    | None -> s
  in

  (* Public wrapper: always land in Overview view on a fresh jump. *)
  let jump_to_result s ri =
    let s' = jump_to_result_raw s ri in
    { s' with history = { s'.history with result_view = Overview } }
  in

  let events = Notty_lwt.Term.events term in

  let rec loop () =
    let* event = Lwt_stream.next events in
    match event with
    | `Key (`ASCII c, mods) when (c = 'C' || c = 'D') && List.mem `Ctrl mods ->
      do_quit ()

    | `Key (`ASCII c, mods) when c = 'V' && List.mem `Ctrl mods ->
      let* () = update mvar (fun s -> { s with verbose = not s.verbose }) in
      redraw mvar; loop ()

    | `Key (`ASCII 'g', mods) when List.mem `Meta mods ->
      let* () = switch_to_git mvar in loop ()

    | `Key (`ASCII 'h', mods) when List.mem `Meta mods ->
      let* () = switch_to_history mvar in loop ()

    (* --- Palette open: intercept keys --- *)
    | `Key (`Enter, _)
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
        let* () = (match cmd with
          | "/git" -> switch_to_git mvar
          | "/history" -> switch_to_history mvar
          | "/verbose" | "/v" ->
            update mvar (fun s ->
              { s with verbose = not s.verbose;
                       status_extra = if s.verbose then "Verbose off" else "Verbose on" })
          | _ ->
            update mvar (fun s ->
              { s with status_extra = Printf.sprintf "Unknown: %s" cmd })
        ) in
        redraw mvar; loop ()
      end

    | `Key (`Escape, _)
      when (match peek mvar with Some s -> s.palette_open | None -> false) ->
      let* () = update mvar (fun s ->
        { s with input_text = ""; palette_open = false; palette_selected = 0 }) in
      redraw mvar; loop ()

    | `Key (`Arrow `Up, _)
      when (match peek mvar with Some s -> s.palette_open | None -> false) ->
      let* () = update mvar (fun s ->
        let n = List.length (filter_commands s.input_text) in
        let sel = if n = 0 then 0
          else (s.palette_selected - 1 + n) mod n in
        { s with palette_selected = sel }) in
      redraw mvar; loop ()

    | `Key (`Arrow `Down, _)
      when (match peek mvar with Some s -> s.palette_open | None -> false) ->
      let* () = update mvar (fun s ->
        let n = List.length (filter_commands s.input_text) in
        let sel = if n = 0 then 0
          else (s.palette_selected + 1) mod n in
        { s with palette_selected = sel }) in
      redraw mvar; loop ()

    | `Key (`Backspace, _)
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

    | `Key (`ASCII c, [])
      when (match peek mvar with Some s -> s.palette_open | None -> false) ->
      let ch = Char.code c in
      let* () =
        if ch >= 32 && ch < 127 then
          let* () = update mvar (fun s ->
            { s with input_text = s.input_text ^ String.make 1 c;
                     palette_selected = 0 }) in
          (redraw mvar; Lwt.return_unit)
        else Lwt.return_unit
      in
      loop ()

    (* --- Git mode keys --- *)
    | `Key (`Escape, _)
      when (match peek mvar with Some s -> s.mode = Git | None -> false) ->
      let* () = update mvar (fun s ->
        match s.git.file_diff_filter with
        | Some _ ->
          { s with git = { s.git with file_diff_filter = None; diff_scroll_git = 0 };
                   status_extra = "Git browser" }
        | None ->
          { s with mode = History; status_extra = "Ready" }) in
      redraw mvar; loop ()

    (* Tab: next panel *)
    | `Key (`Tab, mods)
      when (match peek mvar with Some s -> s.mode = Git | None -> false)
        && not (List.mem `Shift mods) ->
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
    | `Key (`Tab, mods)
      when (match peek mvar with Some s -> s.mode = Git | None -> false)
        && List.mem `Shift mods ->
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

    (* Up / k / Down / j: navigate. files + link_candidates are
       derived on read via [current_files] / [current_links], so
       changing commit_idx or file_idx auto-refreshes dependent
       panels on the next render. *)
    | `Key (`Arrow `Up, _)
      when (match peek mvar with Some s -> s.mode = Git | None -> false) ->
      let* () = update mvar (fun s ->
        let g = s.git in
        let git = match g.focus with
          | Branches -> { g with branch_idx = max 0 (g.branch_idx - 1) }
          | Commits -> { g with commit_idx = max 0 (g.commit_idx - 1); file_idx = 0; link_idx = 0 }
          | Files -> { g with file_idx = max 0 (g.file_idx - 1); link_idx = 0 }
          | Links -> { g with link_idx = max 0 (g.link_idx - 1) } in
        { s with git }) in
      redraw mvar; loop ()

    | `Key (`ASCII 'k', [])
      when (match peek mvar with Some s -> s.mode = Git | None -> false) ->
      let* () = update mvar (fun s ->
        let g = s.git in
        let git = match g.focus with
          | Branches -> { g with branch_idx = max 0 (g.branch_idx - 1) }
          | Commits -> { g with commit_idx = prev_linked_commit g g.commit_idx; file_idx = 0; link_idx = 0 }
          | Files -> { g with file_idx = prev_linked_file g g.file_idx; link_idx = 0 }
          | Links -> { g with link_idx = max 0 (g.link_idx - 1) } in
        { s with git }) in
      redraw mvar; loop ()

    | `Key (`Arrow `Down, _)
      when (match peek mvar with Some s -> s.mode = Git | None -> false) ->
      let* () = update mvar (fun s ->
        let g = s.git in
        let files_n = List.length (current_files g) in
        let git = match g.focus with
          | Branches -> { g with branch_idx = min (List.length g.branches - 1) (g.branch_idx + 1) }
          | Commits ->
            { g with commit_idx = min (List.length g.commits - 1) (g.commit_idx + 1);
                     file_idx = 0; link_idx = 0 }
          | Files ->
            { g with file_idx = min (max 0 (files_n - 1)) (g.file_idx + 1); link_idx = 0 }
          | Links -> { g with link_idx = min (max 0 (List.length (current_links g) - 1)) (g.link_idx + 1) } in
        { s with git }) in
      redraw mvar; loop ()

    | `Key (`ASCII 'j', [])
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

    (* [ / ]: scroll diff up/down *)
    | `Key (`ASCII '[', [])
      when (match peek mvar with Some s -> s.mode = Git | None -> false) ->
      let* () = update mvar (fun s ->
        { s with git = { s.git with diff_scroll_git = max 0 (s.git.diff_scroll_git - 1) } }) in
      redraw mvar; loop ()

    | `Key (`ASCII ']', [])
      when (match peek mvar with Some s -> s.mode = Git | None -> false) ->
      let* () = update mvar (fun s ->
        let g = s.git in
        let n = List.length (String.split_on_char '\n' g.diff_preview) in
        { s with git = { g with diff_scroll_git = min (max 0 (n - 1)) (g.diff_scroll_git + 1) } }) in
      redraw mvar; loop ()

    (* h: switch to history — if file has links, show them as a navigable list *)
    | `Key (`ASCII 'h', [])
      when (match peek mvar with Some s -> s.mode = Git | None -> false) ->
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
          let links_unsorted = match List.nth_opt g.commits g.commit_idx, List.nth_opt g.files g.file_idx with
            | Some (sha, _, _), Some file ->
              (match Hashtbl.find_opt g.git_links (sha, Filename.basename file) with
               | Some l -> l | None -> [])
            | _ -> [] in
          let links = List.sort (fun (a : git_conv_link) (b : git_conv_link) ->
            let c = Int.compare a.turn_idx b.turn_idx in
            if c <> 0 then c else Int.compare a.entry_idx b.entry_idx
          ) links_unsorted in
          (* Convert links to search_results format *)
          let results = List.mapi (fun i (link : git_conv_link) ->
            let label = Printf.sprintf "t%d e%d %s"
              link.turn_idx link.entry_idx link.edit_key in
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
                               showing_results = true;
                               result_view = Overview;
                               search_synthesis = "";
                               searching = false };
                   git = { g with link_candidates = links; link_idx = 0 };
                   status_extra = Printf.sprintf "%d links" (List.length links) }) in
        redraw mvar; loop ()
      end else begin
        let* () = switch_to_history mvar in loop ()
      end

    (* Enter: context-dependent activate *)
    | `Key (`Enter, _)
      when (match peek mvar with Some s -> s.mode = Git | None -> false) ->
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
               let turns = split_into_interaction_turns ~filepath:path in
               let ti = min (max 0 (link.turn_idx - 1)) (max 0 (List.length turns - 1)) in
               let new_session_idx =
                 let target = link.session_id ^ ".jsonl" in
                 let rec find i = function
                   | [] -> s.history.session_idx
                   | p :: rest ->
                     if Filename.basename p = target then i else find (i + 1) rest
                 in find 0 s.history.sessions in
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
    | `Key (`Escape, _)
      when (match peek mvar with
            | Some s -> s.mode = History && s.history.search_active | _ -> false) ->
      let* () = update mvar (fun s ->
        { s with history = { s.history with search_active = false; search_query = "" };
                 status_extra = "History" }) in
      redraw mvar; loop ()

    | `Key (`Enter, _)
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

    | `Key (`Backspace, _)
      when (match peek mvar with
            | Some s -> s.mode = History && s.history.search_active | _ -> false) ->
      let* () = update mvar (fun s ->
        let q = s.history.search_query in
        let q' = if String.length q > 0
          then String.sub q 0 (String.length q - 1) else "" in
        { s with history = { s.history with search_query = q' } }) in
      redraw mvar; loop ()

    | `Key (`ASCII c, [])
      when (match peek mvar with
            | Some s -> s.mode = History && s.history.search_active | _ -> false) ->
      let ch = Char.code c in
      if ch >= 32 && ch < 127 then begin
        let* () = update mvar (fun s ->
          let q = s.history.search_query ^ String.make 1 c in
          { s with history = { s.history with search_query = q } }) in
        redraw mvar; loop ()
      end else loop ()

    (* --- History mode: showing search results --- *)
    | `Key (`Escape, _)
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

    | `Key (`Enter, _)
      when (match peek mvar with
            | Some s -> s.mode = History && s.history.showing_results | _ -> false) ->
      let* () = update mvar (fun s ->
        jump_to_result s s.history.result_idx) in
      redraw mvar; loop ()

    (* Overview → Body: Enter promotes from metadata panel to full turn.
       Works for both search-result and plain history-turn navigation. *)
    | `Key (`Enter, _)
      when (match peek mvar with
            | Some s ->
              s.mode = History
              && not s.history.search_active
              && not s.history.showing_results
              && s.history.result_view = Overview
            | None -> false) ->
      let* () = update mvar (fun s ->
        { s with history = { s.history with result_view = Body };
                 status_extra = "Turn body — ←/→ prev/next, b/Enter=overview" }) in
      redraw mvar; loop ()

    (* Body → Overview: Enter snaps back up. *)
    | `Key (`Enter, _)
      when (match peek mvar with
            | Some s ->
              s.mode = History
              && not s.history.search_active
              && not s.history.showing_results
              && s.history.result_view = Body
            | None -> false) ->
      let* () = update mvar (fun s ->
        { s with history = { s.history with result_view = Overview };
                 status_extra = "Overview" }) in
      redraw mvar; loop ()

    (* Plain Right in Overview/Body (search-result mode) → next result,
       snapping to Overview. *)
    | `Key (`Arrow `Right, mods)
      when not (List.mem `Shift mods)
        && (match peek mvar with
            | Some s ->
              s.mode = History
              && s.history.search_results <> []
              && not s.history.showing_results
            | None -> false) ->
      let* () = update mvar (fun s ->
        let max_ri = max 0 (List.length s.history.search_results - 1) in
        let ri = min max_ri (s.history.result_idx + 1) in
        if ri = s.history.result_idx then s
        else jump_to_result s ri) in
      redraw mvar; loop ()

    | `Key (`Arrow `Left, mods)
      when not (List.mem `Shift mods)
        && (match peek mvar with
            | Some s ->
              s.mode = History
              && s.history.search_results <> []
              && not s.history.showing_results
            | None -> false) ->
      let* () = update mvar (fun s ->
        let ri = max 0 (s.history.result_idx - 1) in
        if ri = s.history.result_idx then s
        else jump_to_result s ri) in
      redraw mvar; loop ()

    | `Key (`Arrow `Up, _)
      when (match peek mvar with
            | Some s -> s.mode = History && s.history.showing_results | _ -> false) ->
      let* () = update mvar (fun s ->
        { s with history = { s.history with
            result_idx = max 0 (s.history.result_idx - 1) } }) in
      redraw mvar; loop ()

    | `Key (`Arrow `Down, _)
      when (match peek mvar with
            | Some s -> s.mode = History && s.history.showing_results | _ -> false) ->
      let* () = update mvar (fun s ->
        let max_idx = max 0 (List.length s.history.search_results - 1) in
        { s with history = { s.history with
            result_idx = min max_idx (s.history.result_idx + 1) } }) in
      redraw mvar; loop ()

    | `Key (`ASCII '/', [])
      when (match peek mvar with
            | Some s -> s.mode = History && s.history.showing_results | _ -> false) ->
      let* () = update mvar (fun s ->
        { s with history = { s.history with search_active = true; search_query = "";
                                             showing_results = false } }) in
      redraw mvar; loop ()

    (* --- History mode: normal browsing --- *)
    | `Key (`Escape, _)
      when (match peek mvar with Some s -> s.mode = History | None -> false) ->
      let* () = update mvar (fun s ->
        { s with mode = s.history.return_mode; status_extra = "Ready" }) in
      redraw mvar; loop ()

    | `Key (`ASCII 'g', [])
      when (match peek mvar with Some s -> s.mode = History | None -> false) ->
      let* () = switch_to_git mvar in loop ()

    | `Key (`ASCII 'q', [])
      when (match peek mvar with Some s -> s.mode = History || s.mode = Git | None -> false) ->
      do_quit ()

    | `Key (`ASCII '/', [])
      when (match peek mvar with Some s -> s.mode = History | None -> false) ->
      let* () = update mvar (fun s ->
        { s with history = { s.history with search_active = true; search_query = "" } }) in
      redraw mvar; loop ()

    | `Key (`ASCII 'u', [])
      when (match peek mvar with Some s -> s.mode = History | None -> false) ->
      let* () = index_sessions mvar in
      loop ()

    | `Key (`ASCII 'i', [])
      when (match peek mvar with Some s -> s.mode = History | None -> false) ->
      (* Wipe the V2 SQLite store and re-index from JSONL. *)
      let* () = update mvar (fun s ->
        { s with status_extra = "Initialising (wiping DB)..." }) in
      redraw mvar;
      Lwt.async (fun () ->
        Lwt.catch (fun () ->
          let* s_val = Lwt_mvar.take mvar in
          let* () = Lwt_mvar.put mvar s_val in
          let db_path = Urme_store.Schema.default_path
              ~project_dir:s_val.project_dir in
          List.iter (fun suf ->
            let p = db_path ^ suf in
            if Sys.file_exists p then (try Sys.remove p with _ -> ())
          ) ["-wal"; "-shm"; ""];
          let state_path = Urme_engine.Git_state.state_path
              ~project_dir:s_val.project_dir in
          (try Sys.remove state_path with _ -> ());
          let* () = update mvar (fun s ->
            { s with status_extra = "Initialising: indexing..." }) in
          redraw mvar;
          index_sessions mvar
        ) (fun exn ->
          let* () = update mvar (fun s ->
            { s with status_extra = Printf.sprintf "Wipe error: %s"
                (Printexc.to_string exn) }) in
          redraw mvar; Lwt.return_unit));
      loop ()

    (* Shift+Left/Right: jump to prev/next result (search or git link) *)
    | `Key (`Arrow `Left, mods)
      when List.mem `Shift mods
        && (match peek mvar with
            | Some s -> s.mode = History && s.history.search_results <> []
                        && not s.history.showing_results | _ -> false) ->
      let* () = update mvar (fun s ->
        let ri = max 0 (s.history.result_idx - 1) in
        if ri = s.history.result_idx then s
        else jump_to_result s ri) in
      redraw mvar; loop ()

    | `Key (`Arrow `Right, mods)
      when List.mem `Shift mods
        && (match peek mvar with
            | Some s -> s.mode = History && s.history.search_results <> []
                        && not s.history.showing_results | _ -> false) ->
      let* () = update mvar (fun s ->
        let max_ri = max 0 (List.length s.history.search_results - 1) in
        let ri = min max_ri (s.history.result_idx + 1) in
        if ri = s.history.result_idx then s
        else jump_to_result s ri) in
      redraw mvar; loop ()

    (* b: back to results list *)
    | `Key (`ASCII 'b', [])
      when (match peek mvar with
            | Some s -> s.mode = History && s.history.search_results <> []
                        && not s.history.showing_results | _ -> false) ->
      let* () = update mvar (fun s ->
        let label = if s.history.return_mode = Git then
          Printf.sprintf "%d links" (List.length s.history.search_results)
        else Printf.sprintf "Search: \"%s\"" s.history.search_query in
        { s with history = { s.history with showing_results = true };
                 status_extra = label }) in
      redraw mvar; loop ()

    | `Key (`Arrow `Left, _)
      when (match peek mvar with Some s -> s.mode = History | None -> false) ->
      let* () = update mvar (fun s ->
        let h = s.history in
        if h.turn_idx > 0 then
          { s with history = { h with turn_idx = h.turn_idx - 1;
                                       hist_scroll = 0;
                                       result_view = Overview } }
        else
          let si = max 0 (h.session_idx - 1) in
          if si <> h.session_idx then
            let turns = match List.nth_opt h.sessions si with
              | Some path -> split_into_turns (load_session_entries path)
              | None -> [] in
            let ti = max 0 (List.length turns - 1) in
            { s with history = { h with session_idx = si; turns; turn_idx = ti;
                                         hist_scroll = 0;
                                         result_view = Overview } }
          else s) in
      redraw mvar; loop ()

    | `Key (`Arrow `Right, _)
      when (match peek mvar with Some s -> s.mode = History | None -> false) ->
      let* () = update mvar (fun s ->
        let h = s.history in
        if h.turn_idx < List.length h.turns - 1 then
          { s with history = { h with turn_idx = h.turn_idx + 1;
                                       hist_scroll = 0;
                                       result_view = Overview } }
        else
          let max_si = max 0 (List.length h.sessions - 1) in
          let si = min max_si (h.session_idx + 1) in
          if si <> h.session_idx then
            let turns = match List.nth_opt h.sessions si with
              | Some path -> split_into_turns (load_session_entries path)
              | None -> [] in
            { s with history = { h with session_idx = si; turns; turn_idx = 0;
                                         hist_scroll = 0;
                                         result_view = Overview } }
          else s) in
      redraw mvar; loop ()

    | `Key (`Arrow `Up, _)
      when (match peek mvar with Some s -> s.mode = History | None -> false) ->
      let* () = update mvar (fun s ->
        { s with history = { s.history with
          hist_scroll = max 0 (s.history.hist_scroll - 1) } }) in
      redraw mvar; loop ()

    | `Key (`Arrow `Down, _)
      when (match peek mvar with Some s -> s.mode = History | None -> false) ->
      let* () = update mvar (fun s ->
        { s with history = { s.history with
          hist_scroll = s.history.hist_scroll + 1 } }) in
      redraw mvar; loop ()

    | _ when (match peek mvar with Some s -> s.mode = Git || s.mode = History | None -> false) ->
      loop ()

    | `Resize _ -> redraw mvar; loop ()
    | _ -> loop ()
  in
  loop ()

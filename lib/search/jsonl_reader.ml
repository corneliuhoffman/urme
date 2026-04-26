open Urme_core.Types

(* Compute the JSONL directory for a project *)
let find_jsonl_dir ~project_dir =
  let abs_dir =
    if project_dir = "." || project_dir = "./" then Sys.getcwd ()
    else if Filename.is_relative project_dir then
      Filename.concat (Sys.getcwd ()) project_dir
    else project_dir
  in
  (* Encode path: replace / with - *)
  let encoded = String.map (fun c -> if c = '/' then '-' else c) abs_dir in
  let home = Sys.getenv "HOME" in
  Filename.concat home (Filename.concat ".claude" (Filename.concat "projects" encoded))

(* Read the timestamp from the first line of a JSONL file *)
let first_line_timestamp path =
  try
    let ic = open_in path in
    let line = input_line ic in
    close_in ic;
    let json = Yojson.Safe.from_string line in
    let open Yojson.Safe.Util in
    (match json |> member "timestamp" |> to_string_option with
     | Some ts -> ts
     | None -> "")
  with _ -> ""

(* List session JSONL files sorted by first-message timestamp, newest first *)
let list_sessions ~jsonl_dir =
  if not (Sys.file_exists jsonl_dir) then []
  else
    let entries = Sys.readdir jsonl_dir |> Array.to_list in
    let jsonl_files = List.filter (fun f ->
      Filename.check_suffix f ".jsonl"
    ) entries in
    let with_ts = List.filter_map (fun f ->
      let path = Filename.concat jsonl_dir f in
      if Sys.file_exists path then
        let ts = first_line_timestamp path in
        Some (path, ts)
      else None
    ) jsonl_files in
    let sorted = List.sort (fun (_, t1) (_, t2) ->
      String.compare t2 t1  (* newest first, ISO timestamps sort lexically *)
    ) with_ts in
    List.map fst sorted

(* Get the current (most recent) session JSONL file *)
let current_session ~jsonl_dir =
  match list_sessions ~jsonl_dir with
  | path :: _ -> Some path
  | [] -> None

(* Extract session ID from a JSONL file path *)
let session_id_of_path path =
  Filename.basename path |> Filename.chop_extension

(* A "real" user message is a user-type entry with a plain string
   content that isn't a tool_result *and* isn't synthetic CLI-noise
   like <local-command-stdout>…</…> or <command-name>…. Edit_extract
   uses the same filter, so the indexer's turn numbering now agrees
   with edit_links.turn_idx — otherwise turn_idx=33 in [steps] and
   turn_idx=33 in [edit_links] refer to different turns. *)
let is_real_user_message (json : Yojson.Safe.t) =
  let open Yojson.Safe.Util in
  let typ = json |> member "type" |> to_string_option in
  match typ with
  | Some "user" ->
    let content = json |> member "message" |> member "content" in
    (match content with
     | `String s ->
       let s = String.trim s in
       s <> "" && not (String.length s > 0 && s.[0] = '<')
     | _ -> false)
  | _ -> false

(* Extract user text from a real user message *)
let extract_user_text (json : Yojson.Safe.t) =
  let open Yojson.Safe.Util in
  json |> member "message" |> member "content" |> to_string_option
  |> Option.value ~default:""

(* Extract timestamp from a message *)
let extract_timestamp (json : Yojson.Safe.t) =
  let open Yojson.Safe.Util in
  json |> member "timestamp" |> to_string_option |> Option.value ~default:""

(* Extract git branch from a user message *)
let extract_branch (json : Yojson.Safe.t) =
  let open Yojson.Safe.Util in
  json |> member "gitBranch" |> to_string_option |> Option.value ~default:""

(* Extract UUID from a message *)
let extract_uuid (json : Yojson.Safe.t) =
  let open Yojson.Safe.Util in
  json |> member "uuid" |> to_string_option |> Option.value ~default:""

(* Extract text blocks from assistant messages *)
(* Concatenate assistant output in block order. Thinking blocks are
   labelled `[thinking]` so the summariser can tell reasoning from
   spoken output; it's still the same "assistant produced this"
   stream, just with the hidden layer included. *)
let extract_assistant_text (json : Yojson.Safe.t) =
  let open Yojson.Safe.Util in
  let typ = json |> member "type" |> to_string_option in
  match typ with
  | Some "assistant" ->
    let blocks = json |> member "message" |> member "content"
      |> (fun j -> try to_list j with _ -> []) in
    List.filter_map (fun block ->
      match block |> member "type" |> to_string_option with
      | Some "text" -> block |> member "text" |> to_string_option
      | Some "thinking" ->
        (match block |> member "thinking" |> to_string_option with
         | Some t when t <> "" -> Some ("[thinking]\n" ^ t)
         | _ -> None)
      | _ -> None
    ) blocks
  | _ -> []

(* Extract tool_use file paths from assistant messages *)
let extract_tool_files (json : Yojson.Safe.t) =
  let open Yojson.Safe.Util in
  let typ = json |> member "type" |> to_string_option in
  match typ with
  | Some "assistant" ->
    let blocks = json |> member "message" |> member "content"
      |> (fun j -> try to_list j with _ -> []) in
    List.filter_map (fun block ->
      match block |> member "type" |> to_string_option with
      | Some "tool_use" ->
        let name = block |> member "name" |> to_string_option
          |> Option.value ~default:"" in
        if name = "Edit" || name = "Write" then
          block |> member "input" |> member "file_path" |> to_string_option
        else None
      | _ -> None
    ) blocks
  | _ -> []

(* Extract written content from Edit/Write tool calls in assistant messages.
   Returns (filepath, written_lines) list — the actual text Claude authored. *)
let extract_written_content (json : Yojson.Safe.t) =
  let open Yojson.Safe.Util in
  let typ = json |> member "type" |> to_string_option in
  match typ with
  | Some "assistant" ->
    let blocks = json |> member "message" |> member "content"
      |> (fun j -> try to_list j with _ -> []) in
    List.filter_map (fun block ->
      match block |> member "type" |> to_string_option with
      | Some "tool_use" ->
        let name = block |> member "name" |> to_string_option
          |> Option.value ~default:"" in
        let input = block |> member "input" in
        (match name with
         | "Edit" ->
           let fp = input |> member "file_path" |> to_string_option in
           let ns = input |> member "new_string" |> to_string_option in
           (match fp, ns with
            | Some f, Some s ->
              let lines = String.split_on_char '\n' s
                |> List.filter (fun l -> String.trim l <> "") in
              if lines <> [] then Some (f, lines) else None
            | _ -> None)
         | "Write" ->
           let fp = input |> member "file_path" |> to_string_option in
           let content = input |> member "content" |> to_string_option in
           (match fp, content with
            | Some f, Some s ->
              (* For Write, take first+last 20 non-empty lines as fingerprint *)
              let lines = String.split_on_char '\n' s
                |> List.filter (fun l -> String.trim l <> "") in
              let n = List.length lines in
              let sample = if n <= 40 then lines
                else
                  let first = List.filteri (fun i _ -> i < 20) lines in
                  let last = List.filteri (fun i _ -> i >= n - 20) lines in
                  first @ last in
              if sample <> [] then Some (f, sample) else None
            | _ -> None)
         | _ -> None)
      | _ -> None
    ) blocks
  | _ -> []

(* Read all lines from a file — defined early for use in written_content_for_interaction *)
let read_all_lines_early filepath =
  let ic = open_in filepath in
  let lines = ref [] in
  (try while true do
    lines := input_line ic :: !lines
  done with End_of_file -> ());
  close_in ic;
  List.rev !lines

(* Collect all written content for an interaction's JSONL line range *)
let written_content_for_interaction ~filepath (interaction : Urme_core.Types.interaction) =
  let lines = read_all_lines_early filepath in
  let result = ref [] in
  List.iteri (fun i line ->
    if i >= interaction.line_start && i <= interaction.line_end then
      match (try Some (Yojson.Safe.from_string line) with _ -> None) with
      | Some json ->
        let wc = extract_written_content json in
        result := wc @ !result
      | None -> ()
  ) lines;
  (* Merge entries for the same filepath *)
  let tbl = Hashtbl.create 8 in
  List.iter (fun (fp, lines) ->
    let existing = try Hashtbl.find tbl fp with Not_found -> [] in
    Hashtbl.replace tbl fp (existing @ lines)
  ) !result;
  Hashtbl.fold (fun fp lines acc -> (fp, lines) :: acc) tbl []

(* Extract agentId from tool_result messages (for subagent linking) *)
let extract_agent_ids (json : Yojson.Safe.t) =
  let open Yojson.Safe.Util in
  let typ = json |> member "type" |> to_string_option in
  match typ with
  | Some "user" ->
    let content = json |> member "message" |> member "content" in
    (match content with
     | `List items ->
       List.filter_map (fun item ->
         match item |> member "type" |> to_string_option with
         | Some "tool_result" ->
           let sub_content = item |> member "content"
             |> (fun j -> try to_list j with _ -> []) in
           List.find_map (fun c ->
             match c |> member "type" |> to_string_option with
             | Some "text" ->
               let text = c |> member "text" |> to_string_option
                 |> Option.value ~default:"" in
               (* Look for "agentId: <id>" pattern *)
               (try
                 let prefix = "agentId: " in
                 let idx = String.index text 'a' in
                 let sub = String.sub text idx (String.length text - idx) in
                 if String.length sub > String.length prefix &&
                    String.sub sub 0 (String.length prefix) = prefix then
                   let rest = String.sub sub (String.length prefix)
                     (String.length sub - String.length prefix) in
                   (* Take until space or newline *)
                   let id = try
                     let sp = String.index rest ' ' in
                     String.sub rest 0 sp
                   with Not_found ->
                     try
                       let nl = String.index rest '\n' in
                       String.sub rest 0 nl
                     with Not_found -> rest
                   in
                   Some id
                 else None
               with Not_found -> None)
             | _ -> None
           ) sub_content
         | _ -> None
       ) items
     | _ -> [])
  | _ -> []

(* Read all lines from a file *)
let read_all_lines filepath =
  let ic = open_in filepath in
  let lines = ref [] in
  (try while true do
    lines := input_line ic :: !lines
  done with End_of_file -> ());
  close_in ic;
  List.rev !lines

(* Parse a JSONL file into interactions *)
let parse_interactions ~filepath =
  let lines = read_all_lines filepath in
  let parsed = List.mapi (fun i line ->
    (i, try Some (Yojson.Safe.from_string line) with _ -> None)
  ) lines in
  (* Find real user message boundaries *)
  let user_starts = List.filter_map (fun (line_num, json_opt) ->
    match json_opt with
    | Some json when is_real_user_message json ->
      Some (line_num, json)
    | _ -> None
  ) parsed in
  (* Build interactions from boundaries *)
  let total_lines = List.length lines in
  let rec build_interactions idx starts =
    match starts with
    | [] -> []
    | [(line_num, json)] ->
      let line_end = total_lines - 1 in
      [build_one idx line_num line_end json]
    | (line_num, json) :: ((next_line, _) :: _ as rest) ->
      let line_end = next_line - 1 in
      build_one idx line_num line_end json :: build_interactions (idx + 1) rest
  and build_one idx line_start line_end user_json =
    let range_messages = List.filter_map (fun (line_num, json_opt) ->
      if line_num >= line_start && line_num <= line_end then
        match json_opt with Some j -> Some j | None -> None
      else None
    ) parsed in
    let assistant_texts = List.concat_map extract_assistant_text range_messages in
    let assistant_summary = String.concat "\n" assistant_texts in
    let files = List.concat_map extract_tool_files range_messages in
    let unique_files = List.sort_uniq String.compare files in
    {
      index = idx;
      line_start;
      line_end;
      user_uuid = extract_uuid user_json;
      timestamp = extract_timestamp user_json;
      user_text = extract_user_text user_json;
      assistant_summary;
      files_changed = unique_files;
      branch = extract_branch user_json;
    }
  in
  build_interactions 0 user_starts

(* Check if a byte is a valid UTF-8 continuation byte (10xxxxxx) *)
let is_cont b = b >= 0x80 && b < 0xC0

(* Replace non-UTF8 bytes with '?' *)
let sanitize_utf8 s =
  let buf = Buffer.create (String.length s) in
  let len = String.length s in
  let i = ref 0 in
  while !i < len do
    let c = Char.code s.[!i] in
    if c < 0x80 then begin
      Buffer.add_char buf s.[!i]; incr i
    end else if c < 0xC0 then begin
      (* Unexpected continuation byte *)
      Buffer.add_char buf '?'; incr i
    end else if c < 0xE0 then begin
      if !i + 1 < len && is_cont (Char.code s.[!i + 1]) then begin
        Buffer.add_string buf (String.sub s !i 2); i := !i + 2
      end else begin Buffer.add_char buf '?'; incr i end
    end else if c < 0xF0 then begin
      if !i + 2 < len && is_cont (Char.code s.[!i + 1]) && is_cont (Char.code s.[!i + 2]) then begin
        Buffer.add_string buf (String.sub s !i 3); i := !i + 3
      end else begin Buffer.add_char buf '?'; incr i end
    end else if c < 0xF8 then begin
      if !i + 3 < len && is_cont (Char.code s.[!i + 1]) && is_cont (Char.code s.[!i + 2]) && is_cont (Char.code s.[!i + 3]) then begin
        Buffer.add_string buf (String.sub s !i 4); i := !i + 4
      end else begin Buffer.add_char buf '?'; incr i end
    end else begin
      Buffer.add_char buf '?'; incr i
    end
  done;
  Buffer.contents buf

(* Truncate a string with ellipsis, respecting UTF-8 boundaries *)
let truncate n s =
  if String.length s <= n then s
  else
    (* Back up from position n to find a valid UTF-8 character boundary *)
    let pos = ref n in
    while !pos > 0 && is_cont (Char.code s.[!pos]) do
      decr pos
    done;
    (* pos now points to the start of a multi-byte char or an ASCII char.
       If the multi-byte char extends past n, exclude it. *)
    let c = Char.code s.[!pos] in
    let char_len =
      if c < 0x80 then 1
      else if c < 0xE0 then 2
      else if c < 0xF0 then 3
      else 4
    in
    let cut = if !pos + char_len > n then !pos else !pos + char_len in
    String.sub s 0 cut ^ "..."

(* Extract just the conversational content for summarization:
   user text, assistant thinking, assistant text. No tool calls/results. *)
let summarizable_text ~filepath (interaction : interaction) =
  let lines = read_all_lines filepath in
  let buf = Buffer.create 1024 in
  Buffer.add_string buf (Printf.sprintf "User: %s\n\n" interaction.user_text);
  List.iteri (fun i line ->
    if i >= interaction.line_start && i <= interaction.line_end then
      match (try Some (Yojson.Safe.from_string line) with _ -> None) with
      | Some json ->
        let open Yojson.Safe.Util in
        (match json |> member "type" |> to_string_option with
         | Some "assistant" ->
           let blocks = json |> member "message" |> member "content"
             |> (fun j -> try to_list j with _ -> []) in
           List.iter (fun block ->
             match block |> member "type" |> to_string_option with
             | Some "thinking" ->
               let text = block |> member "thinking" |> to_string_option
                 |> Option.value ~default:"" in
               if String.length text > 0 then
                 Buffer.add_string buf (Printf.sprintf "Thinking: %s\n\n" text)
             | Some "text" ->
               let text = block |> member "text" |> to_string_option
                 |> Option.value ~default:"" in
               Buffer.add_string buf (Printf.sprintf "Assistant: %s\n\n" text)
             | _ -> ()
           ) blocks
         | _ -> ())
      | None -> ()
  ) lines;
  sanitize_utf8 (Buffer.contents buf)

(* Read raw JSONL lines for a given range *)
let read_lines ~filepath ~line_start ~line_end =
  let lines = read_all_lines filepath in
  let selected = List.filteri (fun i _ ->
    i >= line_start && i <= line_end
  ) lines in
  String.concat "\n" selected

(* Extract a readable summary of tool_use input *)
let format_tool_input name (input : Yojson.Safe.t) =
  let open Yojson.Safe.Util in
  match name with
  | "Edit" ->
    let fp = input |> member "file_path" |> to_string_option |> Option.value ~default:"?" in
    let old_s = input |> member "old_string" |> to_string_option |> Option.value ~default:"" in
    let new_s = input |> member "new_string" |> to_string_option |> Option.value ~default:"" in
    if old_s = "" && new_s = "" then
      Printf.sprintf "  file: %s" fp
    else
      Printf.sprintf "  file: %s\n--- old\n%s\n+++ new\n%s" fp old_s new_s
  | "Write" ->
    let fp = input |> member "file_path" |> to_string_option |> Option.value ~default:"?" in
    Printf.sprintf "  file: %s" fp
  | "Read" ->
    let fp = input |> member "file_path" |> to_string_option |> Option.value ~default:"?" in
    Printf.sprintf "  file: %s" fp
  | "Bash" ->
    let cmd = input |> member "command" |> to_string_option |> Option.value ~default:"?" in
    Printf.sprintf "  $ %s" (truncate 120 cmd)
  | "Grep" ->
    let pat = input |> member "pattern" |> to_string_option |> Option.value ~default:"?" in
    Printf.sprintf "  pattern: %s" pat
  | "Glob" ->
    let pat = input |> member "pattern" |> to_string_option |> Option.value ~default:"?" in
    Printf.sprintf "  pattern: %s" pat
  | "Task" ->
    let desc = input |> member "description" |> to_string_option |> Option.value ~default:"?" in
    let prompt = input |> member "prompt" |> to_string_option |> Option.value ~default:"" in
    Printf.sprintf "  %s\n  prompt: %s" desc (truncate 150 prompt)
  | _ ->
    let s = Yojson.Safe.to_string input in
    Printf.sprintf "  %s" (truncate 200 s)

(* Extract a readable summary of tool_result content *)
let format_tool_result (content : Yojson.Safe.t) =
  match content with
  | `String s -> s
  | `List items ->
    let parts = List.filter_map (fun item ->
      let open Yojson.Safe.Util in
      match item |> member "type" |> to_string_option with
      | Some "text" ->
        item |> member "text" |> to_string_option
      | _ -> None
    ) items in
    String.concat "\n" parts
  | _ -> Yojson.Safe.to_string content

(* Format an interaction into readable conversation text *)
let format_interaction ~filepath (interaction : interaction) =
  let lines = read_all_lines filepath in
  let buf = Buffer.create 4096 in
  Buffer.add_string buf (Printf.sprintf "=== Interaction %d ===\n" interaction.index);
  Buffer.add_string buf (Printf.sprintf "User: %s\n" interaction.user_text);
  Buffer.add_string buf (Printf.sprintf "Branch: %s | Time: %s\n\n"
    interaction.branch interaction.timestamp);
  List.iteri (fun i line ->
    if i >= interaction.line_start && i <= interaction.line_end then
      match (try Some (Yojson.Safe.from_string line) with _ -> None) with
      | Some json ->
        let open Yojson.Safe.Util in
        let typ = json |> member "type" |> to_string_option in
        (match typ with
         | Some "assistant" ->
           let blocks = json |> member "message" |> member "content"
             |> (fun j -> try to_list j with _ -> []) in
           List.iter (fun block ->
             match block |> member "type" |> to_string_option with
             | Some "thinking" ->
               let text = block |> member "thinking" |> to_string_option
                 |> Option.value ~default:"" in
               if String.length text > 0 then
                 Buffer.add_string buf (Printf.sprintf "[Thinking]: %s\n\n" text)
             | Some "text" ->
               let text = block |> member "text" |> to_string_option
                 |> Option.value ~default:"" in
               Buffer.add_string buf (Printf.sprintf "Assistant: %s\n\n" text)
             | Some "tool_use" ->
               let name = block |> member "name" |> to_string_option
                 |> Option.value ~default:"?" in
               let input = block |> member "input" in
               Buffer.add_string buf (Printf.sprintf "[Tool: %s]\n%s\n\n"
                 name (format_tool_input name input))
             | _ -> ()
           ) blocks
         | Some "user" ->
           (* Tool results *)
           let content = json |> member "message" |> member "content" in
           (match content with
            | `List items ->
              List.iter (fun item ->
                match item |> member "type" |> to_string_option with
                | Some "tool_result" ->
                  let result_content = item |> member "content" in
                  let text = format_tool_result result_content in
                  if String.length text > 0 then
                    Buffer.add_string buf (Printf.sprintf "[Result]: %s\n\n" text)
                | _ -> ()
              ) items
            | _ -> ())
         | _ -> ())
      | None -> ()
  ) lines;
  sanitize_utf8 (Buffer.contents buf)

(* Format with subagent expansion *)
let format_interaction_with_subagents ~jsonl_dir ~filepath (interaction : interaction) =
  let lines = read_all_lines filepath in
  let session_id = session_id_of_path filepath in
  let session_dir = Filename.concat jsonl_dir session_id in
  let subagent_dir = Filename.concat session_dir "subagents" in
  let buf = Buffer.create 4096 in
  Buffer.add_string buf (Printf.sprintf "=== Interaction %d ===\n" interaction.index);
  Buffer.add_string buf (Printf.sprintf "User: %s\n" interaction.user_text);
  Buffer.add_string buf (Printf.sprintf "Branch: %s | Time: %s\n\n"
    interaction.branch interaction.timestamp);
  (* Collect agent IDs from this interaction *)
  let agent_conversations = Hashtbl.create 4 in
  List.iteri (fun i line ->
    if i >= interaction.line_start && i <= interaction.line_end then
      match (try Some (Yojson.Safe.from_string line) with _ -> None) with
      | Some json ->
        let agent_ids = extract_agent_ids json in
        List.iter (fun agent_id ->
          let agent_file = Filename.concat subagent_dir
            (Printf.sprintf "agent-%s.jsonl" agent_id) in
          if Sys.file_exists agent_file then begin
            let agent_lines = read_all_lines agent_file in
            let agent_buf = Buffer.create 1024 in
            List.iter (fun aline ->
              match (try Some (Yojson.Safe.from_string aline) with _ -> None) with
              | Some ajson ->
                let open Yojson.Safe.Util in
                (match ajson |> member "type" |> to_string_option with
                 | Some "assistant" ->
                   let blocks = ajson |> member "message" |> member "content"
                     |> (fun j -> try to_list j with _ -> []) in
                   List.iter (fun block ->
                     match block |> member "type" |> to_string_option with
                     | Some "text" ->
                       let text = block |> member "text" |> to_string_option
                         |> Option.value ~default:"" in
                       Buffer.add_string agent_buf text;
                       Buffer.add_char agent_buf '\n'
                     | Some "tool_use" ->
                       let name = block |> member "name" |> to_string_option
                         |> Option.value ~default:"?" in
                       Buffer.add_string agent_buf
                         (Printf.sprintf "  [Tool: %s]\n" name)
                     | _ -> ()
                   ) blocks
                 | _ -> ())
              | None -> ()
            ) agent_lines;
            Hashtbl.replace agent_conversations agent_id (Buffer.contents agent_buf)
          end
        ) agent_ids
      | None -> ()
  ) lines;
  (* Now format the main interaction with subagent inlining *)
  List.iteri (fun i line ->
    if i >= interaction.line_start && i <= interaction.line_end then
      match (try Some (Yojson.Safe.from_string line) with _ -> None) with
      | Some json ->
        let open Yojson.Safe.Util in
        (match json |> member "type" |> to_string_option with
         | Some "assistant" ->
           let blocks = json |> member "message" |> member "content"
             |> (fun j -> try to_list j with _ -> []) in
           List.iter (fun block ->
             match block |> member "type" |> to_string_option with
             | Some "text" ->
               let text = block |> member "text" |> to_string_option
                 |> Option.value ~default:"" in
               Buffer.add_string buf (Printf.sprintf "Assistant: %s\n\n" text)
             | Some "tool_use" ->
               let name = block |> member "name" |> to_string_option
                 |> Option.value ~default:"?" in
               Buffer.add_string buf (Printf.sprintf "[Tool: %s]\n" name)
             | _ -> ()
           ) blocks
         | Some "user" ->
           (* Check for agent results *)
           let agent_ids = extract_agent_ids json in
           List.iter (fun agent_id ->
             match Hashtbl.find_opt agent_conversations agent_id with
             | Some conv ->
               Buffer.add_string buf
                 (Printf.sprintf "\n  --- Subagent %s ---\n%s  --- End subagent ---\n\n"
                    agent_id conv)
             | None -> ()
           ) agent_ids
         | _ -> ())
      | None -> ()
  ) lines;
  sanitize_utf8 (Buffer.contents buf)


open Lwt.Syntax

(* Claude CLI as a persistent JSON daemon.

   Spawns:  claude --print --input-format stream-json --output-format stream-json --verbose
   The process stays alive with stdin open. We send user messages as NDJSON on
   stdin and read events from stdout. Claude holds full conversation context
   in memory — no need to re-spawn per message.

   Permission prompts are routed through an MCP tool (permission bridge) that
   communicates with the TUI via Unix domain socket. *)

type claude_opts = {
  model : string option;
  system_prompt : string option;
  allowed_tools : string list;
  max_turns : int option;
  extra_args : string list;
  (* Permission bridge: when set, generates MCP config and passes
     --mcp-config + --permission-prompt-tool to Claude CLI *)
  permission_bridge_binary : string option;
  permission_socket_path : string option;
}

let default_opts = {
  model = None;
  system_prompt = None;
  allowed_tools = [];
  max_turns = None;
  extra_args = [];
  permission_bridge_binary = None;
  permission_socket_path = None;
}

(* A persistent Claude daemon handle *)
type t = {
  proc : Lwt_process.process_full;
  events : Stream.stream_event Lwt_stream.t;
  push : Stream.stream_event option -> unit;
  mutable session_id : string option;
  mcp_config_path : string option;
}

(* Write MCP config to a temp file, return path *)
let write_mcp_config ~bridge_binary ~socket_path =
  let config = Yojson.Safe.to_string (`Assoc [
    "mcpServers", `Assoc [
      "urme_perms", `Assoc [
        "type", `String "stdio";
        "command", `String bridge_binary;
        "args", `List [`String socket_path];
      ]
    ]
  ]) in
  let path = Filename.temp_file "urme-mcp-" ".json" in
  let oc = open_out path in
  output_string oc config;
  close_out oc;
  path

(* Build flags for persistent daemon mode *)
let daemon_flags ~opts =
  let args = ["--print";
              "--input-format"; "stream-json";
              "--output-format"; "stream-json";
              "--verbose"] in
  let args = match opts.model with
    | Some m -> args @ ["--model"; m]
    | None -> args in
  let args = match opts.system_prompt with
    | Some s -> args @ ["--system-prompt"; s]
    | None -> args in
  let args = match opts.max_turns with
    | Some n -> args @ ["--max-turns"; string_of_int n]
    | None -> args in
  let args = List.fold_left (fun acc tool ->
    acc @ ["--allowedTools"; tool]
  ) args opts.allowed_tools in
  args @ opts.extra_args

(* Strip ANTHROPIC_API_KEY (use Max subscription) and CLAUDECODE (no nesting) *)
let clean_env () =
  Unix.environment ()
  |> Array.to_list
  |> List.filter (fun s ->
    let starts_with prefix =
      String.length s >= String.length prefix &&
      String.sub s 0 (String.length prefix) = prefix in
    not (starts_with "ANTHROPIC_API_KEY=") &&
    not (starts_with "CLAUDECODE=") &&
    not (starts_with "CLAUDE_CODE_ENTRYPOINT="))
  |> Array.of_list

(* Write a JSON line to stdin *)
let write_json t json =
  let line = Yojson.Safe.to_string json ^ "\n" in
  let* () = Lwt_io.write t.proc#stdin line in
  Lwt_io.flush t.proc#stdin

(* Read lines from stdout, parse events *)
let reader_loop t =
  let rec loop () =
    let* line = Lwt.catch
      (fun () -> Lwt_io.read_line_opt t.proc#stdout)
      (fun _exn -> Lwt.return_none) in
    match line with
    | Some line ->
      (match Stream.parse_line line with
       | Some (Stream.System_init { session_id; _ } as event) ->
         t.session_id <- Some session_id;
         t.push (Some event)
       | Some event ->
         t.push (Some event)
       | None -> ());
      loop ()
    | None ->
      t.push None;
      Lwt.return_unit
  in
  loop ()

(* Clean up temp MCP config file *)
let cleanup_mcp_config t =
  match t.mcp_config_path with
  | Some path -> (try Unix.unlink path with Unix.Unix_error _ -> ())
  | None -> ()

(* Spawn the persistent Claude daemon.
   When permission_bridge_binary and permission_socket_path are set,
   generates MCP config and passes --permission-prompt-tool. *)
let spawn ?(cwd=".") ?(opts=default_opts) ~binary () =
  let flags = daemon_flags ~opts in
  let mcp_config_path, flags =
    match opts.permission_bridge_binary, opts.permission_socket_path with
    | Some bridge_bin, Some sock_path ->
      let cfg_path = write_mcp_config ~bridge_binary:bridge_bin
          ~socket_path:sock_path in
      let extra = ["--mcp-config"; cfg_path;
                   "--permission-prompt-tool";
                   "mcp__urme_perms__prompt_permission"] in
      (Some cfg_path, flags @ extra)
    | _ ->
      (None, flags)
  in
  let args = [binary] @ flags in
  let cmd = (binary, Array.of_list args) in
  let env = clean_env () in
  let proc = Lwt_process.open_process_full ~cwd ~env cmd in
  let events, push = Lwt_stream.create () in
  let t = { proc; events; push; session_id = None; mcp_config_path } in
  Lwt.async (fun () -> reader_loop t);
  Lwt.return t

(* Send a user message to the daemon *)
let send t ~text =
  let json = `Assoc [
    "type", `String "user";
    "message", `Assoc [
      "role", `String "user";
      "content", `String text;
    ];
  ] in
  write_json t json

(* Read the next event from the stream *)
let next_event t =
  Lwt_stream.get t.events

(* Iterate over all events *)
let iter_events t ~f =
  Lwt_stream.iter_s f t.events

(* Collect all text until the stream ends *)
let collect_text t =
  let buf = Buffer.create 256 in
  let* () = Lwt_stream.iter (function
    | Stream.Assistant_message { content; _ } ->
      Buffer.add_string buf (Stream.text_of_content content)
    | _ -> ()
  ) t.events in
  Lwt.return (Buffer.contents buf)

(* Wait for the process to finish *)
let wait t =
  cleanup_mcp_config t;
  t.proc#close

(* Kill the daemon *)
let kill t =
  cleanup_mcp_config t;
  t.proc#kill Sys.sigterm

(* One-shot spawn for `urme ask` — runs with -p, no daemon stdin loop *)
let spawn_oneshot ?(cwd=".") ?(opts=default_opts) ~binary ~prompt () =
  let args = ["--print";
              "--output-format"; "stream-json";
              "--verbose";
              "-p"; prompt] in
  let args = match opts.model with
    | Some m -> args @ ["--model"; m]
    | None -> args in
  let args = match opts.system_prompt with
    | Some s -> args @ ["--system-prompt"; s]
    | None -> args in
  let args = match opts.max_turns with
    | Some n -> args @ ["--max-turns"; string_of_int n]
    | None -> args in
  let args = List.fold_left (fun acc tool ->
    acc @ ["--allowedTools"; tool]
  ) args opts.allowed_tools in
  let args = args @ opts.extra_args in
  let cmd_args = [binary] @ args in
  let cmd = (binary, Array.of_list cmd_args) in
  let env = clean_env () in
  let proc = Lwt_process.open_process_full ~cwd ~env cmd in
  let events, push = Lwt_stream.create () in
  let t = { proc; events; push; session_id = None; mcp_config_path = None } in
  Lwt.async (fun () -> reader_loop t);
  Lwt.return t

(* Check if still running *)
let is_running t =
  match Lwt.state (t.proc#status) with
  | Lwt.Sleep -> true
  | _ -> false

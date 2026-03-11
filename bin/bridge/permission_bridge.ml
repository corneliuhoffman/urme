open Lwt.Syntax

(* Minimal MCP server over stdio (JSON-RPC 2.0).
   Exposes one tool: prompt_permission.
   When called, connects to the urme TUI via Unix socket to get Y/N approval.
   Takes the TUI socket path as argv[1]. *)

let tui_socket_path = ref ""

(* Debug log to /tmp/urme-bridge.log *)
let log_file = lazy (open_out_gen [Open_creat; Open_append; Open_wronly] 0o644
                       "/tmp/urme-bridge.log")
let debug fmt =
  Printf.ksprintf (fun s ->
    let oc = Lazy.force log_file in
    Printf.fprintf oc "[%s] %s\n%!" (string_of_float (Unix.gettimeofday ())) s
  ) fmt

(* --- JSON helpers --- *)

let str json key =
  let open Yojson.Safe.Util in
  try json |> member key |> to_string with _ -> ""

let int_opt json key =
  let open Yojson.Safe.Util in
  try Some (json |> member key |> to_int) with _ -> None

(* --- stdio I/O --- *)

let read_line_stdin () =
  Lwt.catch
    (fun () -> Lwt_io.read_line_opt Lwt_io.stdin)
    (fun _exn -> Lwt.return_none)

let write_stdout json =
  let line = Yojson.Safe.to_string json ^ "\n" in
  let* () = Lwt_io.write Lwt_io.stdout line in
  Lwt_io.flush Lwt_io.stdout

(* --- JSON-RPC response builders --- *)

let jsonrpc_result id result =
  `Assoc [
    "jsonrpc", `String "2.0";
    "id", `Int id;
    "result", result;
  ]

let jsonrpc_error id code message =
  `Assoc [
    "jsonrpc", `String "2.0";
    "id", `Int id;
    "error", `Assoc [
      "code", `Int code;
      "message", `String message;
    ];
  ]

(* --- MCP handlers --- *)

let handle_initialize id =
  write_stdout (jsonrpc_result id (`Assoc [
    "protocolVersion", `String "2024-11-05";
    "capabilities", `Assoc ["tools", `Assoc []];
    "serverInfo", `Assoc [
      "name", `String "urme-permission-bridge";
      "version", `String "0.1.0";
    ];
  ]))

let handle_tools_list id =
  let tool = `Assoc [
    "name", `String "prompt_permission";
    "description", `String "Prompt the user for permission to use a tool";
    "inputSchema", `Assoc [
      "type", `String "object";
      "properties", `Assoc [
        "tool_name", `Assoc [
          "type", `String "string";
          "description", `String "Name of the tool requiring permission";
        ];
        "input", `Assoc [
          "type", `String "object";
          "description", `String "Tool input parameters";
        ];
      ];
      "required", `List [`String "tool_name"; `String "input"];
    ];
  ] in
  write_stdout (jsonrpc_result id (`Assoc [
    "tools", `List [tool];
  ]))

(* Connect to TUI socket, send request, get Y/N response *)
let prompt_tui ~tool_name ~tool_input =
  debug "prompt_tui: tool=%s socket=%s" tool_name !tui_socket_path;
  debug "prompt_tui: socket file exists=%b"
    (Sys.file_exists !tui_socket_path);
  Lwt.catch (fun () ->
    let socket = Lwt_unix.socket Unix.PF_UNIX Unix.SOCK_STREAM 0 in
    let addr = Unix.ADDR_UNIX !tui_socket_path in
    (* 30 second timeout for connection + response *)
    let timeout = Lwt_unix.sleep 30.0 |> Lwt.map (fun () ->
      debug "prompt_tui: TIMEOUT waiting for TUI response";
      false) in
    let action =
      let* () = Lwt_unix.connect socket addr in
      debug "prompt_tui: connected to socket";
      let ic = Lwt_io.of_fd ~mode:Lwt_io.Input socket in
      let oc = Lwt_io.of_fd ~mode:Lwt_io.Output socket in
      let msg = `Assoc [
        "tool_name", `String tool_name;
        "input", tool_input;
      ] in
      let* () = Lwt_io.write_line oc (Yojson.Safe.to_string msg) in
      let* () = Lwt_io.flush oc in
      debug "prompt_tui: sent request, waiting for response";
      let* response_line = Lwt_io.read_line ic in
      debug "prompt_tui: got response: %s" response_line;
      let* () = Lwt_unix.close socket in
      let response = Yojson.Safe.from_string response_line in
      let open Yojson.Safe.Util in
      let allow = try response |> member "allow" |> to_bool with _ -> false in
      Lwt.return allow
    in
    Lwt.pick [action; timeout]
  ) (fun exn ->
    debug "prompt_tui: EXCEPTION: %s" (Printexc.to_string exn);
    Lwt.return false
  )

let handle_tools_call id params =
  let open Yojson.Safe.Util in
  let name = try params |> member "name" |> to_string with _ -> "" in
  if name <> "prompt_permission" then
    write_stdout (jsonrpc_error id (-32602) "Unknown tool")
  else begin
    let args = try params |> member "arguments" with _ -> `Null in
    let tool_name = str args "tool_name" in
    let tool_input = try args |> member "input" with _ -> `Null in
    let* allow = prompt_tui ~tool_name ~tool_input in
    let behavior = if allow then "allow" else "deny" in
    let result_text = if allow then
      Yojson.Safe.to_string (`Assoc [
        "behavior", `String behavior;
        "updatedInput", tool_input;
      ])
    else
      Yojson.Safe.to_string (`Assoc [
        "behavior", `String behavior;
        "message", `String "User denied permission";
      ])
    in
    write_stdout (jsonrpc_result id (`Assoc [
      "content", `List [
        `Assoc ["type", `String "text"; "text", `String result_text];
      ];
      "isError", `Bool false;
    ]))
  end

(* --- Main loop --- *)

let handle_message line =
  debug "recv: %s" line;
  match Yojson.Safe.from_string line with
  | json ->
    let method_ = str json "method" in
    let id = int_opt json "id" in
    (match method_, id with
     | "initialize", Some id -> handle_initialize id
     | "notifications/initialized", _ -> Lwt.return_unit
     | "tools/list", Some id -> handle_tools_list id
     | "tools/call", Some id ->
       let params = try Yojson.Safe.Util.member "params" json
         with _ -> `Null in
       handle_tools_call id params
     | _, Some id ->
       write_stdout (jsonrpc_error id (-32601) "Method not found")
     | _, None -> Lwt.return_unit)
  | exception _ -> Lwt.return_unit

let () =
  if Array.length Sys.argv < 2 then begin
    Printf.eprintf "Usage: permission_bridge <socket-path>\n";
    exit 1
  end;
  tui_socket_path := Sys.argv.(1);
  debug "bridge started, socket_path=%s, argv=%s" !tui_socket_path
    (String.concat " " (Array.to_list Sys.argv));
  Lwt_main.run begin
    let rec loop () =
      let* line = read_line_stdin () in
      match line with
      | None -> Lwt.return_unit
      | Some l ->
        let* () = handle_message l in
        loop ()
    in
    loop ()
  end

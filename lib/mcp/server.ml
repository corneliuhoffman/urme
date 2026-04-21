(* MCP server — JSON-RPC 2.0 over stdio *)

open Lwt.Syntax

let handle_message state (msg : Yojson.Safe.t) =
  let open Yojson.Safe.Util in
  let id = msg |> member "id" in
  let method_ = msg |> member "method" |> to_string_option
    |> Option.value ~default:"" in
  match method_ with
  | "initialize" ->
    let result = `Assoc [
      "protocolVersion", `String "2024-11-05";
      "capabilities", `Assoc [
        "tools", `Assoc [];
      ];
      "serverInfo", `Assoc [
        "name", `String "urme";
        "version", `String "0.2.0";
      ];
    ] in
    Lwt.return (`Assoc [
      "jsonrpc", `String "2.0";
      "id", id;
      "result", result;
    ])
  | "notifications/initialized" ->
    Lwt.return `Null
  | "tools/list" ->
    let result = `Assoc ["tools", Tools.tool_definitions] in
    Lwt.return (`Assoc [
      "jsonrpc", `String "2.0";
      "id", id;
      "result", result;
    ])
  | "tools/call" ->
    let params = msg |> member "params" in
    let name = params |> member "name" |> to_string in
    let args = params |> member "arguments" in
    let* result =
      Lwt.catch
        (fun () -> Handlers.dispatch state name args)
        (fun exn ->
          Lwt.return (`Assoc [
            "content", `List [
              `Assoc ["type", `String "text";
                      "text", `String (Printf.sprintf "Error: %s"
                        (Printexc.to_string exn))];
            ];
            "isError", `Bool true;
          ]))
    in
    Lwt.return (`Assoc [
      "jsonrpc", `String "2.0";
      "id", id;
      "result", result;
    ])
  | "" -> Lwt.return `Null
  | m when String.length m >= 14 && String.sub m 0 14 = "notifications/" ->
    Lwt.return `Null
  | _ ->
    Lwt.return (`Assoc [
      "jsonrpc", `String "2.0";
      "id", id;
      "error", `Assoc [
        "code", `Int (-32601);
        "message", `String (Printf.sprintf "Method not found: %s" method_);
      ];
    ])

let run ~project_dir =
  let state = Handlers.create_state ~project_dir in
  let stdin = Lwt_io.stdin in
  let stdout = Lwt_io.stdout in
  let rec loop () =
    let* line = Lwt_io.read_line_opt stdin in
    match line with
    | None -> Lwt.return_unit
    | Some line ->
      let line = String.trim line in
      if String.length line = 0 then loop ()
      else begin
        let* () =
          Lwt.catch
            (fun () ->
              let msg = Yojson.Safe.from_string line in
              let* response = handle_message state msg in
              match response with
              | `Null -> Lwt.return_unit
              | resp ->
                let resp_str = Yojson.Safe.to_string resp in
                let* () = Lwt_io.write_line stdout resp_str in
                Lwt_io.flush stdout)
            (fun exn ->
              Lwt_io.eprintf "urme-mcp error: %s\nInput: %s\n"
                (Printexc.to_string exn)
                (String.sub line 0 (min 200 (String.length line))))
        in
        loop ()
      end
  in
  loop ()

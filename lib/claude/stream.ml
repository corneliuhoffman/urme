(* Claude CLI stream-json NDJSON parser.

   Persistent daemon mode:
     claude --print --input-format stream-json --output-format stream-json --verbose

   Stdout events:
   - {"type":"system","subtype":"init","session_id":"...","model":"...","tools":[...],...}
   - {"type":"assistant","message":{"role":"assistant","content":[...],...},...}
   - {"type":"user","message":{"role":"user","content":[...],...},...}
   - {"type":"result","subtype":"success|error","result":"...","duration_ms":...,...}
   - {"type":"control_request","request_id":"...","request":{"subtype":"can_use_tool",...}}

   Stdin messages:
   - {"type":"user","message":{"role":"user","content":"..."}}
   - {"type":"control_response","request_id":"...","response":{"subtype":"success",...}}
*)

(* Content blocks inside messages *)
type content_block =
  | Text of string
  | Thinking of string
  | Tool_use of { id : string; name : string; input : Yojson.Safe.t }
  | Tool_result of { tool_use_id : string; content : string; is_error : bool }

(* Usage statistics *)
type usage = {
  input_tokens : int;
  output_tokens : int;
  cache_creation_input_tokens : int;
  cache_read_input_tokens : int;
}

let empty_usage = {
  input_tokens = 0; output_tokens = 0;
  cache_creation_input_tokens = 0; cache_read_input_tokens = 0;
}

(* Control request from Claude CLI asking for permission *)
type control_request = {
  request_id : string;
  subtype : string;          (* "can_use_tool", etc. *)
  tool_name : string;
  tool_input : Yojson.Safe.t;
}

(* Stream events *)
type stream_event =
  | System_init of {
      session_id : string;
      cwd : string;
      model : string;
      tools : string list;
    }
  | Assistant_message of {
      session_id : string;
      uuid : string;
      message_id : string;
      model : string;
      content : content_block list;
      stop_reason : string;
      usage : usage;
      error : string option;
    }
  | User_message of {
      session_id : string;
      uuid : string;
      content : content_block list;
    }
  | Result of {
      session_id : string;
      uuid : string;
      subtype : string;
      is_error : bool;
      result : string;
      duration_ms : int;
      num_turns : int;
      total_cost_usd : float;
      usage : usage;
    }
  | Control_request of control_request
  | Unknown of Yojson.Safe.t

let parse_usage json =
  let open Yojson.Safe.Util in
  let int_field key =
    try json |> member key |> to_int with _ -> 0 in
  {
    input_tokens = int_field "input_tokens";
    output_tokens = int_field "output_tokens";
    cache_creation_input_tokens = int_field "cache_creation_input_tokens";
    cache_read_input_tokens = int_field "cache_read_input_tokens";
  }

let parse_content_block json =
  let open Yojson.Safe.Util in
  let typ = json |> member "type" |> to_string_option
            |> Option.value ~default:"" in
  match typ with
  | "text" ->
    Text (json |> member "text" |> to_string_option
          |> Option.value ~default:"")
  | "thinking" ->
    Thinking (json |> member "thinking" |> to_string_option
              |> Option.value ~default:"")
  | "tool_use" ->
    Tool_use {
      id = json |> member "id" |> to_string_option
           |> Option.value ~default:"";
      name = json |> member "name" |> to_string_option
             |> Option.value ~default:"";
      input = (try json |> member "input" with _ -> `Null);
    }
  | "tool_result" ->
    Tool_result {
      tool_use_id = json |> member "tool_use_id" |> to_string_option
                    |> Option.value ~default:"";
      content = json |> member "content" |> to_string_option
                |> Option.value ~default:"";
      is_error = (try json |> member "is_error" |> to_bool with _ -> false);
    }
  | _ -> Text ""

let parse_content_blocks json =
  let open Yojson.Safe.Util in
  try json |> to_list |> List.map parse_content_block
  with _ -> []

let str json key =
  let open Yojson.Safe.Util in
  json |> member key |> to_string_option |> Option.value ~default:""

let parse_line line : stream_event option =
  try
    let json = Yojson.Safe.from_string line in
    let open Yojson.Safe.Util in
    let typ = str json "type" in
    match typ with
    | "system" ->
      let subtype = str json "subtype" in
      if subtype = "init" then
        Some (System_init {
          session_id = str json "session_id";
          cwd = str json "cwd";
          model = str json "model";
          tools = (try json |> member "tools" |> to_list
                       |> List.map to_string
                   with _ -> []);
        })
      else Some (Unknown json)
    | "assistant" ->
      let msg = json |> member "message" in
      let usage_json = msg |> member "usage" in
      Some (Assistant_message {
        session_id = str json "session_id";
        uuid = str json "uuid";
        message_id = str msg "id";
        model = str msg "model";
        content = parse_content_blocks (msg |> member "content");
        stop_reason = str msg "stop_reason";
        usage = parse_usage usage_json;
        error = json |> member "error" |> to_string_option;
      })
    | "user" ->
      let msg = json |> member "message" in
      Some (User_message {
        session_id = str json "session_id";
        uuid = str json "uuid";
        content = parse_content_blocks (msg |> member "content");
      })
    | "result" ->
      let usage_json = json |> member "usage" in
      Some (Result {
        session_id = str json "session_id";
        uuid = str json "uuid";
        subtype = str json "subtype";
        is_error = (try json |> member "is_error" |> to_bool with _ -> false);
        result = str json "result";
        duration_ms = (try json |> member "duration_ms" |> to_int with _ -> 0);
        num_turns = (try json |> member "num_turns" |> to_int with _ -> 0);
        total_cost_usd =
          (try json |> member "total_cost_usd" |> to_float with _ -> 0.0);
        usage = parse_usage usage_json;
      })
    | "control_request" ->
      let req = json |> member "request" in
      Some (Control_request {
        request_id = str json "request_id";
        subtype = str req "subtype";
        tool_name = str req "tool_name";
        tool_input = (try req |> member "input" with _ -> `Null);
      })
    | _ -> Some (Unknown json)
  with _ -> None

(* Extract text content from an assistant message *)
let text_of_content blocks =
  List.filter_map (function
    | Text s -> Some s
    | _ -> None
  ) blocks
  |> String.concat ""

(* Check if an event indicates an error *)
let is_error = function
  | Assistant_message { error = Some _; _ } -> true
  | Result { is_error = true; _ } -> true
  | _ -> false

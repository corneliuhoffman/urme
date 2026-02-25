open Lwt.Syntax
open Types

(* Tool definitions for tools/list *)
let tool_definitions = `List [
  `Assoc [
    "name", `String "search_experience";
    "description", `String "Search past saved experiences for relevant prior work. Use at the start of a task to find similar past solutions. Returns matching experiences with their full conversation transcripts.";
    "inputSchema", `Assoc [
      "type", `String "object";
      "properties", `Assoc [
        "query", `Assoc [
          "type", `String "string";
          "description", `String "Natural language description of what you're looking for";
        ];
        "n_results", `Assoc [
          "type", `String "integer";
          "description", `String "Number of results to return (default 3)";
        ];
        "broad_k", `Assoc [
          "type", `String "integer";
          "description", `String "Number of broad candidates to consider before re-ranking (default 10)";
        ];
      ];
      "required", `List [`String "query"];
    ];
  ];
  `Assoc [
    "name", `String "save_step";
    "description", `String "Save the current state as a checkpoint. Creates a git micro-commit and stores the experience (including the full conversation transcript) in the vector DB for future retrieval.";
    "inputSchema", `Assoc [
      "type", `String "object";
      "properties", `Assoc [
        "label", `Assoc [
          "type", `String "string";
          "description", `String "Short description of what was accomplished";
        ];
        "intent", `Assoc [
          "type", `String "string";
          "description", `String "The original user request/intent";
        ];
        "conversation", `Assoc [
          "type", `String "string";
          "description", `String "Full conversation transcript since last save point. Include: user messages, your reasoning, all tool calls with inputs/outputs, errors, and the final outcome.";
        ];
      ];
      "required", `List [`String "label"; `String "intent"; `String "conversation"];
    ];
  ];
  `Assoc [
    "name", `String "go_back";
    "description", `String "Semantic undo. Describe the state you want to revert to in natural language. Searches both current session steps and past saved experiences. Returns the best match for confirmation before applying.";
    "inputSchema", `Assoc [
      "type", `String "object";
      "properties", `Assoc [
        "query", `Assoc [
          "type", `String "string";
          "description", `String "Natural language description of the state to revert to, e.g. 'when you last changed the API' or 'before the auth changes'";
        ];
        "confirm", `Assoc [
          "type", `String "boolean";
          "description", `String "Set to true to confirm and apply the rollback after reviewing the match. First call without confirm to see the match.";
        ];
      ];
      "required", `List [`String "query"];
    ];
  ];
  `Assoc [
    "name", `String "show_steps";
    "description", `String "Show all steps in the current session with their labels and save status.";
    "inputSchema", `Assoc [
      "type", `String "object";
      "properties", `Assoc [];
    ];
  ];
  `Assoc [
    "name", `String "commit";
    "description", `String "Squash all micro-commits into a single clean commit on the original branch.";
    "inputSchema", `Assoc [
      "type", `String "object";
      "properties", `Assoc [
        "message", `Assoc [
          "type", `String "string";
          "description", `String "Commit message for the squashed commit";
        ];
      ];
      "required", `List [`String "message"];
    ];
  ];
  `Assoc [
    "name", `String "list_experiences";
    "description", `String "List all saved experiences for this project.";
    "inputSchema", `Assoc [
      "type", `String "object";
      "properties", `Assoc [
        "limit", `Assoc [
          "type", `String "integer";
          "description", `String "Maximum number of experiences to return (default 10)";
        ];
      ];
    ];
  ];
  `Assoc [
    "name", `String "delete_experience";
    "description", `String "Delete a saved experience by ID.";
    "inputSchema", `Assoc [
      "type", `String "object";
      "properties", `Assoc [
        "id", `Assoc [
          "type", `String "string";
          "description", `String "The experience ID to delete";
        ];
      ];
      "required", `List [`String "id"];
    ];
  ];
]

let text_result text =
  `Assoc [
    "content", `List [
      `Assoc ["type", `String "text"; "text", `String text];
    ];
  ]

let json_result j =
  text_result (Yojson.Safe.pretty_to_string j)

(* State *)
type state = {
  port : int;
  project_dir : string;
  mutable collections : collections option;
  mutable tracker : Step_tracker.t option;
  mutable last_undo_match : search_result option;
}

let get_collections state =
  match state.collections with
  | Some c -> Lwt.return c
  | None ->
    let dir = if state.project_dir = "." || state.project_dir = "./"
      then Sys.getcwd ()
      else if Filename.is_relative state.project_dir
      then Filename.concat (Sys.getcwd ()) state.project_dir
      else state.project_dir in
    let project = Filename.basename dir in
    let* c = Chromadb.ensure_collections ~port:state.port ~project in
    state.collections <- Some c;
    Lwt.return c

let get_tracker state =
  match state.tracker with
  | Some t -> Lwt.return t
  | None ->
    let* sha = Git_ops.get_current_sha ~cwd:state.project_dir in
    let* collections = get_collections state in
    let t = Step_tracker.create ~base_sha:sha ~port:state.port ~collections in
    state.tracker <- Some t;
    Lwt.return t

(* Tool handlers *)
let handle_search_experience state args =
  let open Yojson.Safe.Util in
  let query = args |> member "query" |> to_string in
  let n = args |> member "n_results" |> to_int_option |> Option.value ~default:3 in
  let broad_k = args |> member "broad_k" |> to_int_option |> Option.value ~default:10 in
  let* collections = get_collections state in
  let+ results = Chromadb.search_two_stage ~port:state.port
    ~collections ~query ~broad_k ~n in
  let results_json = `List (List.map experience_to_yojson results) in
  json_result results_json

let handle_save_step state args =
  let open Yojson.Safe.Util in
  let label = args |> member "label" |> to_string in
  let intent = args |> member "intent" |> to_string in
  let conversation = args |> member "conversation" |> to_string in
  let* tracker = get_tracker state in
  let* collections = get_collections state in
  (* Compute diff from last step or base *)
  let from_sha = match Step_tracker.all tracker |> List.rev with
    | last :: _ -> last.commit_sha
    | [] -> Step_tracker.base_sha tracker
  in
  (* Try to create micro-commit, but don't fail if no changes *)
  let step_num = List.length (Step_tracker.all tracker) + 1 in
  let* commit_sha =
    Lwt.catch
      (fun () -> Git_ops.micro_commit ~cwd:state.project_dir ~label ~step_num)
      (fun _exn -> Git_ops.get_current_sha ~cwd:state.project_dir)
  in
  let* diff =
    Lwt.catch
      (fun () -> Git_ops.diff_between ~cwd:state.project_dir
        ~from_sha ~to_sha:commit_sha)
      (fun _exn -> Lwt.return "")
  in
  let* files =
    Lwt.catch
      (fun () -> Git_ops.changed_files ~cwd:state.project_dir ~from_sha)
      (fun _exn -> Lwt.return [])
  in
  (* Add step to tracker (also indexes in session collection) *)
  let* step = Step_tracker.add tracker ~label ~intent ~conversation
    ~commit_sha ~diff ~files in
  (* Save to persistent experience collections *)
  let exp = step_to_experience step in
  let* () = Chromadb.save_experience ~port:state.port ~collections exp in
  let result = `Assoc [
    "step_number", `Int step.number;
    "experience_id", `String step.experience_id;
    "commit_sha", `String commit_sha;
    "files_changed", `List (List.map (fun f -> `String f) files);
  ] in
  Lwt.return (json_result result)

let handle_go_back state args =
  let open Yojson.Safe.Util in
  let query = args |> member "query" |> to_string in
  let confirm = args |> member "confirm" |> to_bool_option
    |> Option.value ~default:false in
  let* tracker = get_tracker state in
  if confirm then begin
    (* Apply the previously found match *)
    match state.last_undo_match with
    | None ->
      Lwt.return (text_result "No pending undo match. Call go_back with a query first.")
    | Some result ->
      let sha = result.experience.commit_sha in
      if sha = "" then
        Lwt.return (text_result "Cannot roll back: no commit SHA associated with this experience.")
      else begin
      let* () = Git_ops.rollback_to ~cwd:state.project_dir ~sha in
      let* () = match result.source with
        | `Session ->
          (match List.find_opt (fun s ->
            s.experience_id = result.experience.id
          ) (Step_tracker.all tracker) with
          | Some step -> Step_tracker.truncate_after tracker step.number
          | None -> Lwt.return_unit)
        | `Experience -> Lwt.return_unit
      in
      state.last_undo_match <- None;
      let+ () = Lwt.return_unit in
      text_result (Printf.sprintf "Rolled back to: %s (commit: %s, source: %s)"
        result.experience.label sha
        (match result.source with `Session -> "session" | `Experience -> "experience"))
      end
  end else begin
    (* Search for matching state *)
    (* First try direct step number *)
    let direct_step =
      try
        let n = int_of_string (String.trim query) in
        Step_tracker.get tracker n
      with _ -> None
    in
    match direct_step with
    | Some step ->
      let result = {
        experience = step_to_experience step;
        source = `Session;
        distance = 0.0;
      } in
      state.last_undo_match <- Some result;
      Lwt.return (json_result (`Assoc [
        "match_found", `Bool true;
        "label", `String step.label;
        "source", `String "session";
        "step_number", `Int step.number;
        "files", `List (List.map (fun f -> `String f) step.files);
        "instruction", `String "Call go_back again with confirm=true to apply this rollback.";
      ]))
    | None ->
      let* collections = get_collections state in
      let* results = Chromadb.search_for_undo ~port:state.port
        ~collections ~query ~n:1 in
      match results with
      | [] ->
        Lwt.return (text_result "No matching state found for that description.")
      | best :: _ ->
        state.last_undo_match <- Some best;
        Lwt.return (json_result (`Assoc [
          "match_found", `Bool true;
          "label", `String best.experience.label;
          "source", `String (match best.source with
            `Session -> "session" | `Experience -> "experience");
          "distance", `Float best.distance;
          "files", `List (List.map (fun f -> `String f) best.experience.files);
          "instruction", `String "Call go_back again with confirm=true to apply this rollback.";
        ]))
  end

let handle_show_steps state _args =
  let* tracker = get_tracker state in
  Lwt.return (text_result (Step_tracker.format tracker))

let handle_commit state args =
  let open Yojson.Safe.Util in
  let message = args |> member "message" |> to_string in
  let* tracker = get_tracker state in
  let base = Step_tracker.base_sha tracker in
  let+ sha = Git_ops.squash_commit ~cwd:state.project_dir
    ~message ~base_sha:base in
  json_result (`Assoc [
    "sha", `String sha;
    "message", `String message;
  ])

let handle_list_experiences state args =
  let open Yojson.Safe.Util in
  let limit = args |> member "limit" |> to_int_option
    |> Option.value ~default:10 in
  let* collections = get_collections state in
  let+ exps = Chromadb.list_all ~port:state.port ~collections ~limit in
  json_result (`List (List.map experience_to_yojson exps))

let handle_delete_experience state args =
  let open Yojson.Safe.Util in
  let id = args |> member "id" |> to_string in
  let* collections = get_collections state in
  let+ () = Chromadb.delete ~port:state.port ~collections ~id in
  text_result (Printf.sprintf "Deleted experience %s" id)

let dispatch_tool state name args =
  match name with
  | "search_experience" -> handle_search_experience state args
  | "save_step" -> handle_save_step state args
  | "go_back" -> handle_go_back state args
  | "show_steps" -> handle_show_steps state args
  | "commit" -> handle_commit state args
  | "list_experiences" -> handle_list_experiences state args
  | "delete_experience" -> handle_delete_experience state args
  | _ -> Lwt.return (text_result (Printf.sprintf "Unknown tool: %s" name))

(* JSON-RPC message handling *)
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
        "name", `String "experience-agent";
        "version", `String "0.1.0";
      ];
    ] in
    Lwt.return (`Assoc [
      "jsonrpc", `String "2.0";
      "id", id;
      "result", result;
    ])
  | "notifications/initialized" ->
    (* Notification, no response needed *)
    Lwt.return `Null
  | "tools/list" ->
    let result = `Assoc ["tools", tool_definitions] in
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
        (fun () -> dispatch_tool state name args)
        (fun exn ->
          let msg = Printexc.to_string exn in
          Lwt.return (text_result (Printf.sprintf "Error: %s" msg)))
    in
    Lwt.return (`Assoc [
      "jsonrpc", `String "2.0";
      "id", id;
      "result", result;
    ])
  | "" ->
    (* No method field — probably a response or ping, ignore *)
    Lwt.return `Null
  | m when String.length m > 0 && m.[0] = '$' ->
    (* Internal notifications like $/progress, ignore *)
    Lwt.return `Null
  | m when String.sub m 0 (min 14 (String.length m)) = "notifications/" ->
    (* Any notification, ignore *)
    Lwt.return `Null
  | _ ->
    let error = `Assoc [
      "code", `Int (-32601);
      "message", `String (Printf.sprintf "Method not found: %s" method_);
    ] in
    Lwt.return (`Assoc [
      "jsonrpc", `String "2.0";
      "id", id;
      "error", error;
    ])

(* Main stdio loop *)
let run ~port ~project_dir =
  let state = {
    port;
    project_dir;
    collections = None;
    tracker = None;
    last_undo_match = None;
  } in
  let stdin = Lwt_io.stdin in
  let stdout = Lwt_io.stdout in
  let rec loop () =
    let* line = Lwt_io.read_line_opt stdin in
    match line with
    | None -> Lwt.return_unit  (* EOF, shutdown *)
    | Some line ->
      let line = String.trim line in
      if String.length line = 0 then loop ()
      else begin
        let* () =
          Lwt.catch
            (fun () ->
              let* () = Lwt_io.eprintf "experience-agent: received: %s\n"
                (String.sub line 0 (min 200 (String.length line))) in
              let msg = Yojson.Safe.from_string line in
              let* response = handle_message state msg in
              match response with
              | `Null -> Lwt.return_unit
              | resp ->
                let resp_str = Yojson.Safe.to_string resp in
                let* () = Lwt_io.write_line stdout resp_str in
                Lwt_io.flush stdout)
            (fun exn ->
              (* Log error to stderr but don't crash *)
              let err = Printexc.to_string exn in
              let bt = Printexc.get_backtrace () in
              Lwt_io.eprintf "experience-agent error: %s\n%s\nInput was: %s\n" err bt
                (String.sub line 0 (min 200 (String.length line))))
        in
        loop ()
      end
  in
  loop ()

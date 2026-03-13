(* Extract edits from Claude JSONL session files *)

open Git_link_types

let read_lines filepath =
  try
    let ic = open_in filepath in
    let ls = ref [] in
    (try while true do ls := input_line ic :: !ls done with End_of_file -> ());
    close_in ic; List.rev !ls
  with _ -> []

let parse_json_safe line =
  try Some (Yojson.Safe.from_string line) with _ -> None

let edits_of_session ~filepath =
  let session_id = Filename.basename filepath |> Filename.chop_extension in
  let lines = read_lines filepath in
  let open Yojson.Safe.Util in
  (* Pass 1: collect refused tool_use_ids *)
  let refused_ids = Hashtbl.create 32 in
  List.iter (fun line ->
    match parse_json_safe line with
    | None -> ()
    | Some json ->
      if json |> member "type" |> to_string_option = Some "user" then
        let items = json |> member "message" |> member "content"
          |> (fun j -> try to_list j with _ -> []) in
        List.iter (fun item ->
          if item |> member "type" |> to_string_option = Some "tool_result" then
            let is_err = item |> member "is_error" |> to_bool_option
              |> Option.value ~default:false in
            if is_err then
              match item |> member "tool_use_id" |> to_string_option with
              | Some tid -> Hashtbl.replace refused_ids tid ()
              | None -> ()
        ) items
  ) lines;
  (* Pass 2: track interaction boundaries, extract edits *)
  let current_iidx = ref 0 in
  let current_branch = ref "" in
  let edits = ref [] in
  List.iter (fun line ->
    match parse_json_safe line with
    | None -> ()
    | Some json ->
      let typ = json |> member "type" |> to_string_option in
      match typ with
      | Some "user" ->
        let content = json |> member "message" |> member "content" in
        (match content with
         | `String _ ->
           incr current_iidx;
           let branch = json |> member "gitBranch" |> to_string_option
             |> Option.value ~default:"" in
           if branch <> "" then current_branch := branch
         | _ -> ())
      | Some "assistant" ->
        let ts_str = json |> member "timestamp" |> to_string_option
          |> Option.value ~default:"" in
        let ts = Urme_core.Types.iso8601_to_epoch ts_str in
        let blocks = json |> member "message" |> member "content"
          |> (fun j -> try to_list j with _ -> []) in
        let ei = ref 0 in
        List.iter (fun block ->
          let entry_idx = !ei in
          incr ei;
          if block |> member "type" |> to_string_option = Some "tool_use" then begin
            let name = block |> member "name" |> to_string_option
              |> Option.value ~default:"" in
            let tuid = block |> member "id" |> to_string_option
              |> Option.value ~default:"" in
            if (name = "Edit" || name = "Write") &&
               not (Hashtbl.mem refused_ids tuid) && ts > 0.0 then begin
              let input = (try block |> member "input" with _ -> `Null) in
              let fp = input |> member "file_path" |> to_string_option
                |> Option.value ~default:"" in
              if fp <> "" then begin
                let file_base = Filename.basename fp in
                let new_string = match name with
                  | "Edit" -> input |> member "new_string" |> to_string_option
                    |> Option.value ~default:""
                  | "Write" -> input |> member "content" |> to_string_option
                    |> Option.value ~default:""
                  | _ -> "" in
                let old_string = match name with
                  | "Edit" -> input |> member "old_string" |> to_string_option
                    |> Option.value ~default:""
                  | _ -> "" in
                if new_string <> "" then begin
                  let ek = make_edit_key ~file_base ~new_string in
                  edits := { edit_key = ek; file_base; new_string; old_string;
                             timestamp = ts; session_id;
                             interaction_index = !current_iidx;
                             turn_idx = !current_iidx; entry_idx;
                             git_branch = !current_branch } :: !edits
                end
              end
            end
          end
        ) blocks
      | _ -> ()
  ) lines;
  !edits

let sort_by_size_desc paths =
  List.sort (fun a b ->
    let size f = try (Unix.stat f).Unix.st_size with _ -> 0 in
    Int.compare (size b) (size a)
  ) paths

let edits_of_sessions ~pool ~project_dir =
  let jsonl_dir = Urme_search.Jsonl_reader.find_jsonl_dir ~project_dir in
  let sessions = sort_by_size_desc (Urme_search.Jsonl_reader.list_sessions ~jsonl_dir) in
  Domainslib.Task.run pool (fun () ->
    let promises = List.map (fun filepath ->
      Domainslib.Task.async pool (fun () -> edits_of_session ~filepath)
    ) sessions in
    let all = List.concat_map (Domainslib.Task.await pool) promises in
    List.sort (fun a b -> Float.compare b.timestamp a.timestamp) all
  )

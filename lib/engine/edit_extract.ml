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

(* Last-occurrence substring search. Returns the index of the start of
   the last match, or None. *)
let find_last_substring s needle =
  let nlen = String.length needle in
  let slen = String.length s in
  if nlen = 0 || nlen > slen then None
  else
    let rec scan i acc =
      if i > slen - nlen then acc
      else if String.sub s i nlen = needle
      then scan (i + 1) (Some i)
      else scan (i + 1) acc
    in scan 0 None

(* Map an absolute Edit/Write [file_path] to a repo-relative path. The
   walker compares this against [git diff --name-only] output (which is
   repo-relative), so leaving a foreign absolute prefix in place breaks
   commit attribution.

   Two sources of absolute paths to handle:
   1. JSONL recorded on this machine: starts with [project_dir/].
      Strip the prefix.
   2. JSONL imported from another machine: starts with a foreign
      absolute prefix (e.g. /Users/dimitris/projects/opengrep/opengrep/...).
      Find the last "/<basename>/" segment and take the suffix — the
      repo basename is usually distinctive enough to disambiguate.
   Paths matching neither (e.g. /tmp/x.ml or another user's
   ~/.claude/...) are returned unchanged; the linker will skip them. *)
let normalize_file_path ~project_dir fp =
  let pd = project_dir ^ "/" in
  let pdlen = String.length pd in
  if String.length fp >= pdlen
     && String.sub fp 0 pdlen = pd
  then String.sub fp pdlen (String.length fp - pdlen)
  else
    let basename = Filename.basename project_dir in
    if basename = "" then fp
    else
      let needle = "/" ^ basename ^ "/" in
      match find_last_substring fp needle with
      | Some pos ->
        let start = pos + String.length needle in
        String.sub fp start (String.length fp - start)
      | None -> fp

let edits_of_session ?(project_dir="") ~filepath () =
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
  (* Start at -1 so the first real user message takes idx 0, matching
     [Jsonl_reader.parse_interactions] which numbers turns 0..N-1.
     Previously we pre-incremented from 0, giving 1..N and making
     edit_links.turn_idx one ahead of steps.turn_index. *)
  let current_iidx = ref (-1) in
  let current_branch = ref "" in
  let edit_in_turn = ref 0 in
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
         | `String s ->
           (* Skip internal noise to match split_into_interaction_turns *)
           let s = String.trim s in
           let is_noise = s = "" ||
             (String.length s > 0 && s.[0] = '<') in
           if not is_noise then begin
             incr current_iidx;
             edit_in_turn := 0;
             let branch = json |> member "gitBranch" |> to_string_option
               |> Option.value ~default:"" in
             if branch <> "" then current_branch := branch
           end
         | _ -> ())
      | Some "assistant" ->
        let ts_str = json |> member "timestamp" |> to_string_option
          |> Option.value ~default:"" in
        let ts = Urme_core.Types.iso8601_to_epoch ts_str in
        let blocks = json |> member "message" |> member "content"
          |> (fun j -> try to_list j with _ -> []) in
        List.iter (fun block ->
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
                let replace_all = match name with
                  | "Edit" -> input |> member "replace_all" |> to_bool_option
                    |> Option.value ~default:false
                  | _ -> false in
                if new_string <> "" then begin
                  let ek = make_edit_key ~file_base ~old_string ~new_string in
                  let eidx = !edit_in_turn in
                  incr edit_in_turn;
                  let file_path = normalize_file_path ~project_dir fp in
                  edits := { edit_key = ek; file_base; file_path;
                             new_string; old_string;
                             replace_all; timestamp = ts; session_id;
                             interaction_index = !current_iidx;
                             turn_idx = !current_iidx; entry_idx = eidx;
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
      Domainslib.Task.async pool (fun () ->
        edits_of_session ~project_dir ~filepath ())
    ) sessions in
    let all = List.concat_map (Domainslib.Task.await pool) promises in
    List.sort (fun a b -> Float.compare b.timestamp a.timestamp) all
  )

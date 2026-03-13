(* Git↔Claude link types and persistence helpers *)

type edit = {
  edit_key : string;          (* file_base:Digest(new_string) *)
  file_base : string;
  new_string : string;
  old_string : string;        (* empty for Write *)
  timestamp : float;
  session_id : string;
  interaction_index : int;
  turn_idx : int;
  entry_idx : int;
  git_branch : string;        (* from JSONL gitBranch *)
}

type link = {
  commit_sha : string;
  file : string;
  session_id : string;
  turn_idx : int;
  entry_idx : int;
  edit_key : string;
}

type conflict_resolution =
  | AcceptOurs
  | AcceptTheirs
  | AcceptBoth

type provenance =
  | DirectEdit of edit
  | Incoming of provenance list * string
  | ConflictChoice of conflict_resolution
  | ConflictResolution of edit
  | Unexplained of string

type decomposition = {
  commit_sha : string;
  file : string;
  items : provenance list;
}

type git_info = {
  commit_sha : string;
  diff_hash : string;
  turn_idx : int;
  entry_idx : int;
}

(* --- Helpers --- *)

let make_edit_key ~file_base ~new_string =
  Printf.sprintf "%s:%s" file_base (Digest.string new_string |> Digest.to_hex)

let diff_hash_of_string s = Digest.string s |> Digest.to_hex

let find_substring needle haystack =
  try Some (Str.search_forward (Str.regexp_string needle) haystack 0)
  with Not_found -> None

(* --- git_info JSON persistence --- *)

let parse_git_info_json s =
  if s = "" then Hashtbl.create 0
  else
    let tbl = Hashtbl.create 16 in
    (try
      match Yojson.Safe.from_string s with
      | `Assoc pairs ->
        List.iter (fun (key, value) ->
          match value with
          | `List [`String sha; `String dh; `Int ti; `Int ei] ->
            Hashtbl.replace tbl key
              (Some { commit_sha = sha; diff_hash = dh; turn_idx = ti; entry_idx = ei })
          | `List [`String sha; `String dh] ->
            Hashtbl.replace tbl key
              (Some { commit_sha = sha; diff_hash = dh; turn_idx = 0; entry_idx = 0 })
          | `Null -> Hashtbl.replace tbl key None
          | _ -> ()
        ) pairs
      | _ -> ()
    with _ -> ());
    tbl

let serialize_git_info_json tbl =
  let pairs = Hashtbl.fold (fun key value acc ->
    let v = match value with
      | Some gi ->
        `List [`String gi.commit_sha; `String gi.diff_hash;
               `Int gi.turn_idx; `Int gi.entry_idx]
      | None -> `Null in
    (key, v) :: acc
  ) tbl [] in
  Yojson.Safe.to_string (`Assoc pairs)

let git_info_is_complete tbl =
  Hashtbl.length tbl > 0 &&
  Hashtbl.fold (fun _k v acc -> acc && v <> None) tbl true

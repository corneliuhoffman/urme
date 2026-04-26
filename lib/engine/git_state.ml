(* Persistent git state snapshot for branch-aware indexing.

   Stored at [.urme/git-state.json] in the project root.  Records the
   tip SHA of each branch at the last update, so a subsequent run can
   diff current vs saved state and only process what changed. *)

module SMap = Map.Make(String)

type branch_snapshot = {
  tip : string;            (* Branch tip SHA at last save *)
  last_processed : string; (* Latest commit actually walked (>= tip if skipped any) *)
}

type t = {
  branches : branch_snapshot SMap.t;
  updated_at : float;
}

let empty = { branches = SMap.empty; updated_at = 0.0 }

let state_dir ~project_dir = Filename.concat project_dir ".urme"
let state_path ~project_dir = Filename.concat (state_dir ~project_dir) "git-state.json"

(* ---------- Serialization ---------- *)

let to_json t =
  let branches = SMap.fold (fun name s acc ->
    (name, `Assoc [
      "tip", `String s.tip;
      "last_processed", `String s.last_processed;
    ]) :: acc
  ) t.branches [] in
  `Assoc [
    "updated_at", `Float t.updated_at;
    "branches", `Assoc branches;
  ]

let of_json json =
  let open Yojson.Safe.Util in
  try
    let updated_at = try json |> member "updated_at" |> to_number with _ -> 0.0 in
    let branches =
      try json |> member "branches" |> to_assoc
      with _ -> [] in
    let branches = List.fold_left (fun acc (name, b) ->
      let tip = try b |> member "tip" |> to_string with _ -> "" in
      let last_processed =
        try b |> member "last_processed" |> to_string
        with _ -> tip in
      SMap.add name { tip; last_processed } acc
    ) SMap.empty branches in
    { branches; updated_at }
  with _ -> empty

(* ---------- IO ---------- *)

let load ~project_dir =
  let path = state_path ~project_dir in
  if not (Sys.file_exists path) then empty
  else
    try
      let ic = open_in path in
      let content = In_channel.input_all ic in
      close_in ic;
      of_json (Yojson.Safe.from_string content)
    with _ -> empty

let save ~project_dir t =
  let dir = state_dir ~project_dir in
  (try Unix.mkdir dir 0o755
   with Unix.Unix_error (Unix.EEXIST, _, _) -> ());
  let path = state_path ~project_dir in
  let content = Yojson.Safe.pretty_to_string (to_json t) in
  let oc = open_out path in
  output_string oc content;
  close_out oc

(* ---------- Manipulation ---------- *)

let get_branch t name = SMap.find_opt name t.branches

let update_branch t name snap =
  { branches = SMap.add name snap t.branches;
    updated_at = Unix.gettimeofday () }

let remove_branch t name =
  { branches = SMap.remove name t.branches;
    updated_at = Unix.gettimeofday () }

let current_branches t =
  SMap.bindings t.branches |> List.map fst

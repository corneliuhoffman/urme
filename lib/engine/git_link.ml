(* Git↔Claude link queries (pure analysis — no storage side-effects).

   The old 3-phase [update_index] that populated ChromaDB is gone; its
   role is now played by [Git_index.update] writing to SQLite. These
   two functions run per-request diff decomposition for the MCP tools. *)

open Git_link_types
open Lwt.Syntax

(* Lwt-based bounded concurrency for IO work *)
let lwt_iter ~n f items =
  let q = Queue.create () in
  List.iter (fun x -> Queue.push x q) items;
  let worker () =
    let rec loop () =
      match Queue.take_opt q with
      | None -> Lwt.return_unit
      | Some item -> let* () = f item in loop ()
    in loop ()
  in
  Lwt.join (List.init (min n (Queue.length q)) (fun _ -> worker ()))

let ncpu =
  try int_of_string (String.trim (Sys.getenv "URME_WORKERS"))
  with _ ->
    try
      let ic = Unix.open_process_in
        "sysctl -n hw.ncpu 2>/dev/null || nproc 2>/dev/null" in
      let n = try int_of_string (String.trim (input_line ic)) with _ -> 4 in
      ignore (Unix.close_process_in ic); max 2 n
    with _ -> 4

(* For each commit that touched [file_path], run decompose_diff. *)
let file_history ~project_dir ~file_path ~edits ~branch_label ~repo =
  let cwd = project_dir in
  let* log = Lwt.catch (fun () ->
    Urme_git.Ops.run_git ~cwd
      ["log"; "--follow"; "--format=%H"; "--"; file_path]
  ) (fun _ -> Lwt.return "") in
  let shas = String.split_on_char '\n' log
    |> List.filter_map (fun s ->
      let s = String.trim s in if s <> "" then Some s else None) in
  let decompositions = ref [] in
  let* () = lwt_iter ~n:(ncpu * 2) (fun sha ->
    let* d = Diff_match.decompose_diff ~sha ~file:file_path
        ~branch_label ~edits ~cwd ~repo in
    if d.items <> [] then decompositions := d :: !decompositions;
    Lwt.return_unit
  ) shas in
  Lwt.return (List.rev !decompositions)

(* Line-range variant: run blame, then decompose each commit that shows up. *)
let region_history ~project_dir ~path ~start_line ~end_line
    ~edits ~branch_label ~repo =
  let cwd = project_dir in
  let* blame_output = Lwt.catch (fun () ->
    Urme_git.Ops.run_git ~cwd
      ["blame"; "-L"; Printf.sprintf "%d,%d" start_line end_line;
       "--porcelain"; "--"; path]
  ) (fun _ -> Lwt.return "") in
  let shas = Hashtbl.create 16 in
  String.split_on_char '\n' blame_output |> List.iter (fun line ->
    if String.length line >= 40 then
      let c = line.[0] in
      if (c >= '0' && c <= '9') || (c >= 'a' && c <= 'f') then
        Hashtbl.replace shas (String.sub line 0 40) ()
  );
  let sha_list = Hashtbl.fold (fun sha () acc -> sha :: acc) shas [] in
  let decompositions = ref [] in
  let* () = lwt_iter ~n:(min ncpu (max 1 (List.length sha_list))) (fun sha ->
    let* d = Diff_match.decompose_diff ~sha ~file:path
        ~branch_label ~edits ~cwd ~repo in
    if d.items <> [] then decompositions := d :: !decompositions;
    Lwt.return_unit
  ) sha_list in
  Lwt.return !decompositions

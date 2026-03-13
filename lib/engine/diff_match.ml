(* Content-based matching: decompose a git diff into attributed Claude edits *)

open Git_link_types
open Lwt.Syntax

(* Match edits against file content by undoing them backwards (newest first).
   Only matches edits that explain the delta from content_before to content_after.
   Returns matched edits in application order (oldest first). *)
let match_edits ~content_before ~content_after ~commit_ts ~file_base ~branch_filter edits =
  let candidates = edits
    |> List.filter (fun e ->
      e.file_base = file_base &&
      e.timestamp < commit_ts &&
      (branch_filter = "" || e.git_branch = branch_filter))
    |> List.sort (fun a b -> Float.compare b.timestamp a.timestamp) in
  let current = ref content_after in
  let matched = ref [] in
  List.iter (fun edit ->
    if edit.old_string <> "" then begin
      (* Edit with old_string — find new_string, undo by replacing back *)
      let cc = !current in
      match find_substring edit.new_string cc with
      | None -> ()
      | Some _ when find_substring edit.new_string content_before <> None ->
        (* new_string already in parent content — this edit predates this commit *)
        ()
      | Some pos ->
        let ns_len = String.length edit.new_string in
        let cc_len = String.length cc in
        let undone = String.sub cc 0 pos ^ edit.old_string ^
                     String.sub cc (pos + ns_len) (cc_len - pos - ns_len) in
        let fname = file_base in
        (match Patch.diff (Some (fname, cc)) (Some (fname, undone)) with
         | Some reverse_patch ->
           (match (try Patch.patch ~cleanly:true (Some cc) reverse_patch
                   with _ -> None) with
            | Some new_cc -> current := new_cc; matched := edit :: !matched
            | None -> current := undone; matched := edit :: !matched)
         | None ->
           current := undone; matched := edit :: !matched)
    end else if edit.new_string <> "" then begin
      (* Write — check if content sample is present in after but not before *)
      let ns = edit.new_string in
      let cc = !current in
      let check_len = min (String.length ns) (String.length cc) in
      if check_len > 20 then
        let sample = String.sub ns 0 (min 100 (String.length ns)) in
        match find_substring sample cc with
        | Some _ when find_substring sample content_before = None ->
          matched := edit :: !matched
        | _ -> ()
    end
  ) candidates;
  !matched

(* Decompose a single commit's diff for a file into provenance *)
let decompose_diff ~sha ~file ~branch_label ~edits ~cwd ~repo =
  let file_base = Filename.basename file in
  let branch = match Hashtbl.find_opt branch_label sha with
    | Some b -> b | None -> "" in
  let* content_after = Lwt.catch (fun () ->
    Urme_store.Project_store.read_blob ~repo ~sha ~path:file
  ) (fun _ -> Lwt.return_none) in
  match content_after with
  | None -> Lwt.return { commit_sha = sha; file = file_base; items = [] }
  | Some content_after ->
    let* ts_str = Lwt.catch (fun () ->
      Urme_git.Ops.run_git ~cwd ["log"; "-1"; "--format=%at"; sha]
    ) (fun _ -> Lwt.return "0") in
    let commit_ts = try float_of_string (String.trim ts_str) with _ -> 0.0 in
    (* Check if merge commit *)
    let* parents_str = Lwt.catch (fun () ->
      Urme_git.Ops.run_git ~cwd ["log"; "-1"; "--format=%P"; sha]
    ) (fun _ -> Lwt.return "") in
    let parents = String.split_on_char ' ' (String.trim parents_str)
      |> List.filter (fun s -> s <> "") in
    (* Get content before (from first parent) *)
    let* content_before = match parents with
      | parent :: _ ->
        Lwt.catch (fun () ->
          let+ c = Urme_store.Project_store.read_blob ~repo ~sha:parent ~path:file in
          Option.value c ~default:""
        ) (fun _ -> Lwt.return "")
      | [] -> Lwt.return "" in
    if List.length parents <= 1 then begin
      (* Regular commit *)
      let matched = match_edits ~content_before ~content_after ~commit_ts ~file_base
        ~branch_filter:branch edits in
      Lwt.return { commit_sha = sha; file = file_base;
                   items = List.map (fun e -> DirectEdit e) matched }
    end else begin
      (* Merge commit — attribute ours vs incoming *)
      let theirs_parent = List.nth parents 1 in
      let theirs_branch = match Hashtbl.find_opt branch_label theirs_parent with
        | Some b -> b | None -> "" in
      let ours = match_edits ~content_before ~content_after ~commit_ts ~file_base
        ~branch_filter:branch edits in
      let theirs = match_edits ~content_before ~content_after ~commit_ts ~file_base
        ~branch_filter:theirs_branch edits in
      let items =
        List.map (fun e -> DirectEdit e) ours @
        (if theirs <> [] then
           [Incoming (List.map (fun e -> DirectEdit e) theirs, theirs_branch)]
         else []) in
      Lwt.return { commit_sha = sha; file = file_base; items }
    end

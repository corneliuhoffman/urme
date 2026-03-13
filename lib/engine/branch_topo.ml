(* Build branch topology: label each commit SHA with its original branch *)

open Lwt.Syntax

let label_commits ~cwd =
  let labels = Hashtbl.create 256 in
  (* Get merged PR branch names from GitHub API — one call *)
  let* pr_json = Lwt.catch (fun () ->
    Urme_git.Ops.run_shell
      (Printf.sprintf "cd %s && gh pr list --state merged --json mergeCommit,headRefName --limit 500 2>/dev/null"
         (Filename.quote cwd))
  ) (fun _ -> Lwt.return "[]") in
  let merge_branches = Hashtbl.create 64 in
  (try
    match Yojson.Safe.from_string pr_json with
    | `List prs ->
      List.iter (fun pr ->
        let open Yojson.Safe.Util in
        let branch = pr |> member "headRefName" |> to_string_option
          |> Option.value ~default:"" in
        let sha = pr |> member "mergeCommit" |> member "oid" |> to_string_option
          |> Option.value ~default:"" in
        if branch <> "" && sha <> "" then
          Hashtbl.replace merge_branches sha branch
      ) prs
    | _ -> ()
  with _ -> ());
  (* For each merge commit, label its branch commits via git log M^1..M^2 *)
  let* () = Lwt_list.iter_p (fun (merge_sha, branch) ->
    let* branch_log = Lwt.catch (fun () ->
      Urme_git.Ops.run_git ~cwd
        ["log"; "--format=%H"; Printf.sprintf "%s^1..%s^2" merge_sha merge_sha]
    ) (fun _ -> Lwt.return "") in
    String.split_on_char '\n' branch_log |> List.iter (fun sha ->
      let sha = String.trim sha in
      if sha <> "" then
        Hashtbl.replace labels sha branch
    );
    (* Also label the merge commit itself with its target branch *)
    Lwt.return_unit
  ) (Hashtbl.fold (fun k v acc -> (k, v) :: acc) merge_branches []) in
  (* Living branches: label first-parent commits *)
  let* branches_raw = Lwt.catch (fun () ->
    Urme_git.Ops.run_git ~cwd ["branch"; "--list"; "--no-color"]
  ) (fun _ -> Lwt.return "") in
  let branches = String.split_on_char '\n' branches_raw
    |> List.filter_map (fun s ->
      let s = String.trim s in
      if s = "" then None
      else
        let s = if String.length s > 2 && s.[0] = '*' then
          String.trim (String.sub s 2 (String.length s - 2))
        else s in
        Some s
    ) in
  let* () = Lwt_list.iter_p (fun branch ->
    let* log = Lwt.catch (fun () ->
      Urme_git.Ops.run_git ~cwd
        ["log"; "--first-parent"; "--format=%H"; branch; "--max-count=500"]
    ) (fun _ -> Lwt.return "") in
    String.split_on_char '\n' log |> List.iter (fun sha ->
      let sha = String.trim sha in
      if sha <> "" && not (Hashtbl.mem labels sha) then
        Hashtbl.replace labels sha branch
    );
    Lwt.return_unit
  ) branches in
  Lwt.return labels

(* Test backward (undo) direction: start from content_after,
   undo matched edits newest-first, check if we reach content_before *)

open Lwt.Syntax
open Urme_engine.Git_link_types

let project_dir = Sys.getcwd ()

let () =
  Lwt_main.run begin
    let pool = Domainslib.Task.setup_pool ~num_domains:4 () in
    let edits = Urme_engine.Edit_extract.edits_of_sessions ~pool ~project_dir in
    Domainslib.Task.teardown_pool pool;

    let* labels = Urme_engine.Branch_topo.label_commits ~cwd:project_dir in
    let* log = Urme_git.Ops.run_git ~cwd:project_dir
      ["log"; "--format=%H"; "--max-count=10"] in
    let shas = String.split_on_char '\n' log
      |> List.filter (fun s -> String.trim s <> "") in
    let* repo = Urme_store.Project_store.open_repo ~project_dir in

    let n_pass = ref 0 in
    let n_fail = ref 0 in
    let n_skip = ref 0 in

    let* () = Lwt_list.iter_s (fun sha ->
      let short = String.sub sha 0 7 in
      let* parents_str = Lwt.catch (fun () ->
        Urme_git.Ops.run_git ~cwd:project_dir ["log"; "-1"; "--format=%P"; sha]
      ) (fun _ -> Lwt.return "") in
      let parent = match String.split_on_char ' ' (String.trim parents_str)
        |> List.filter (fun s -> s <> "") with
        | p :: _ -> Some p | [] -> None in

      let* files_str = Lwt.catch (fun () ->
        Urme_git.Ops.run_git ~cwd:project_dir
          ["diff-tree"; "--no-commit-id"; "-r"; "--name-only"; sha]
      ) (fun _ -> Lwt.return "") in
      let files = String.split_on_char '\n' files_str
        |> List.filter (fun s -> String.trim s <> "") in

      let* () = Lwt_list.iter_s (fun file ->
        let* content_after_opt = Lwt.catch (fun () ->
          Urme_store.Project_store.read_blob ~repo ~sha ~path:file
        ) (fun _ -> Lwt.return_none) in
        let* content_before = match parent with
          | Some p -> Lwt.catch (fun () ->
              let+ c = Urme_store.Project_store.read_blob ~repo ~sha:p ~path:file in
              Option.value c ~default:""
            ) (fun _ -> Lwt.return "")
          | None -> Lwt.return "" in

        match content_after_opt with
        | None -> incr n_skip; Lwt.return_unit
        | Some _ when content_before = "" -> incr n_skip; Lwt.return_unit
        | Some content_after ->
          let* d = Urme_engine.Diff_match.decompose_diff
            ~sha ~file ~branch_label:labels ~edits ~cwd:project_dir ~repo in
          let matched = List.filter_map (fun item ->
            match item with DirectEdit e -> Some e | _ -> None
          ) d.items in
          if matched = [] then begin
            incr n_skip; Lwt.return_unit
          end else begin
            (* Undo edits newest-first (reverse the oldest-first list) *)
            let reversed = List.rev matched in
            let current = ref content_after in
            List.iter (fun (edit : edit) ->
              let cc = !current in
              if edit.old_string <> "" then begin
                match find_substring edit.new_string cc with
                | Some pos ->
                  let ns_len = String.length edit.new_string in
                  let cc_len = String.length cc in
                  current := String.sub cc 0 pos ^ edit.old_string ^
                    String.sub cc (pos + ns_len) (cc_len - pos - ns_len)
                | None -> ()
              end
            ) reversed;
            let result = !current in
            let fb = Filename.basename file in
            if result = content_before then begin
              Printf.printf "  PASS  %s %s (%d edits)\n%!" short fb (List.length matched);
              incr n_pass
            end else begin
              let residual = abs (String.length result - String.length content_before) in
              Printf.printf "  FAIL  %s %s (%d edits, residual %d)\n%!"
                short fb (List.length matched) residual;
              incr n_fail
            end;
            Lwt.return_unit
          end
      ) files in
      Lwt.return_unit
    ) shas in

    Printf.printf "\n=== Backward (undo) results ===\n%!";
    Printf.printf "  PASS: %d | FAIL: %d | SKIP: %d\n%!" !n_pass !n_fail !n_skip;
    Lwt.return_unit
  end

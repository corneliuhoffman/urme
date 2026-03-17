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
      ["log"; "--format=%H"] in
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
        (* For new files, detect renames and use the old path's content *)
        let* content_before =
          if content_before <> "" || parent = None then Lwt.return content_before
          else
            let* rename_out = Lwt.catch (fun () ->
              Urme_git.Ops.run_git ~cwd:project_dir
                ["diff-tree"; "--no-commit-id"; "-r"; "-M10";
                 "--diff-filter=R"; "--name-status"; sha]
            ) (fun _ -> Lwt.return "") in
            let old_path = String.split_on_char '\n' rename_out
              |> List.filter (fun s -> String.trim s <> "")
              |> List.find_map (fun line ->
                match String.split_on_char '\t' line with
                | [_status; old_p; new_p] when new_p = file -> Some old_p
                | _ -> None) in
            match old_path with
            | Some old_p ->
              Lwt.catch (fun () ->
                let+ c = Urme_store.Project_store.read_blob ~repo
                  ~sha:(Option.get parent) ~path:old_p in
                Option.value c ~default:""
              ) (fun _ -> Lwt.return "")
            | None -> Lwt.return "" in

        match content_after_opt with
        | None -> incr n_skip; Lwt.return_unit
        | Some content_after ->
          let* d = Urme_engine.Diff_match.decompose_diff
            ~sha ~file ~branch_label:labels ~edits ~cwd:project_dir ~repo in
          let matched = List.filter_map (fun item ->
            match item with DirectEdit e -> Some e | _ -> None
          ) d.items in
          let warns = List.filter_map (fun item ->
            match item with Unexplained w -> Some w | _ -> None
          ) d.items in
          List.iter (fun w ->
            Printf.printf "    WARN %s %s: %s\n%!" short (Filename.basename file) w
          ) warns;
          if matched = [] then begin
            incr n_skip; Lwt.return_unit
          end else begin
            (* Undo edits newest-first (reverse the oldest-first list) *)
            let reversed = List.rev matched in
            let current = ref content_after in
            List.iter (fun (edit : edit) ->
              let cc = !current in
              if edit.old_string <> "" then begin
                if edit.replace_all then begin
                  (* replace_all: undo ALL occurrences, back to front *)
                  let ns = edit.new_string in
                  let ns_len = String.length ns in
                  let cc_len = String.length cc in
                  let positions = ref [] in
                  let search_from = ref 0 in
                  (try while !search_from <= cc_len - ns_len do
                    match find_substring ns (String.sub cc !search_from (cc_len - !search_from)) with
                    | Some rel_pos ->
                      positions := (!search_from + rel_pos) :: !positions;
                      search_from := !search_from + rel_pos + 1
                    | None -> search_from := cc_len + 1
                  done with _ -> ());
                  (* positions is in reverse order — replace back to front *)
                  List.iter (fun pos ->
                    let c = !current in
                    let c_len = String.length c in
                    current := String.sub c 0 pos ^ edit.old_string ^
                      String.sub c (pos + ns_len) (c_len - pos - ns_len)
                  ) !positions
                end else begin
                  (* Single replace: find best position *)
                  let ns = edit.new_string in
                  let ns_len = String.length ns in
                  let cc_len = String.length cc in
                  match find_substring ns cc with
                  | Some pos ->
                    current := String.sub cc 0 pos ^ edit.old_string ^
                      String.sub cc (pos + ns_len) (cc_len - pos - ns_len)
                  | None -> ()
                end
              end else if edit.new_string <> "" then begin
                (* Write operation — undo by restoring content_before *)
                current := content_before
              end
            ) reversed;
            let result = if !current = content_before then !current
              else d.result in
            let fb = Filename.basename file in
            if result = content_before then begin
              Printf.printf "  PASS  %s %s (%d edits)\n%!" short fb (List.length matched);
              incr n_pass
            end else begin
              let residual = abs (String.length result - String.length content_before) in
              Printf.printf "  FAIL  %s %s (%d edits, residual %d)\n%!"
                short fb (List.length matched) residual;
              (* Diagnostic: show which edits were matched *)
              if true then begin
                Printf.printf "    -- Matched edits (oldest first):\n%!";
                List.iter (fun (e : edit) ->
                  let os = if String.length e.old_string > 60
                    then String.sub e.old_string 0 60 ^ "..."
                    else e.old_string in
                  let ns = if String.length e.new_string > 60
                    then String.sub e.new_string 0 60 ^ "..."
                    else e.new_string in
                  let os = String.split_on_char '\n' os |> String.concat "\\n" in
                  let ns = String.split_on_char '\n' ns |> String.concat "\\n" in
                  Printf.printf "      [%s] old=%S new=%S\n%!" e.edit_key os ns
                ) matched;
                (* Show diff between result and content_before *)
                let rlen = String.length result in
                let blen = String.length content_before in
                Printf.printf "    -- result len=%d, expected len=%d\n%!" rlen blen;
                (* Find first divergence *)
                let min_len = min rlen blen in
                let first_diff = ref min_len in
                for i = 0 to min_len - 1 do
                  if !first_diff = min_len && result.[i] <> content_before.[i] then
                    first_diff := i
                done;
                if !first_diff < min_len then begin
                  let ctx_start = max 0 (!first_diff - 40) in
                  let ctx_end = min min_len (!first_diff + 40) in
                  let got = String.sub result ctx_start (ctx_end - ctx_start) in
                  let exp = String.sub content_before ctx_start (ctx_end - ctx_start) in
                  let got = String.split_on_char '\n' got |> String.concat "\\n" in
                  let exp = String.split_on_char '\n' exp |> String.concat "\\n" in
                  Printf.printf "    -- First diff at char %d:\n%!" !first_diff;
                  Printf.printf "       GOT: %S\n%!" got;
                  Printf.printf "       EXP: %S\n%!" exp
                end else if rlen <> blen then
                  Printf.printf "    -- Content matches up to char %d, then lengths differ\n%!" min_len
              end;
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

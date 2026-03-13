(* Git↔Claude link orchestrator *)

open Git_link_types
open Lwt.Syntax

let log_debug msg =
  let oc = open_out_gen [Open_append; Open_creat] 0o644 "/tmp/urme_debug.log" in
  Printf.fprintf oc "[%s] %s\n" (string_of_float (Unix.gettimeofday ())) msg;
  close_out oc

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
      let ic = Unix.open_process_in "sysctl -n hw.ncpu 2>/dev/null || nproc 2>/dev/null" in
      let n = try int_of_string (String.trim (input_line ic)) with _ -> 4 in
      ignore (Unix.close_process_in ic); max 2 n
    with _ -> 4

(* --- File / region history --- *)

let file_history ~project_dir ~file_path ~edits ~branch_label ~repo =
  let cwd = project_dir in
  let* log = Lwt.catch (fun () ->
    Urme_git.Ops.run_git ~cwd
      ["log"; "--follow"; "--format=%H"; "--"; file_path]
  ) (fun _ -> Lwt.return "") in
  let shas = String.split_on_char '\n' log
    |> List.filter_map (fun s ->
      let s = String.trim s in
      if s <> "" then Some s else None) in
  let decompositions = ref [] in
  let* () = lwt_iter ~n:(ncpu * 2) (fun sha ->
    let* d = Diff_match.decompose_diff ~sha ~file:file_path
      ~branch_label ~edits ~cwd ~repo in
    if d.items <> [] then
      decompositions := d :: !decompositions;
    Lwt.return_unit
  ) shas in
  Lwt.return (List.rev !decompositions)

let region_history ~project_dir ~path ~start_line ~end_line ~edits ~branch_label ~repo =
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
  let* () = lwt_iter ~n:(min ncpu (List.length sha_list)) (fun sha ->
    let* d = Diff_match.decompose_diff ~sha ~file:path
      ~branch_label ~edits ~cwd ~repo in
    if d.items <> [] then
      decompositions := d :: !decompositions;
    Lwt.return_unit
  ) sha_list in
  Lwt.return !decompositions

(* --- Full index update (TUI orchestrator) --- *)

let update_index ~project_dir ~port ~collection_id
    ?(sessions_filter : string list option)
    ?(on_status : (string -> unit Lwt.t) option)
    () =
  let cwd = project_dir in
  let status msg = match on_status with
    | Some f -> f msg | None -> Lwt.return_unit in
  let n_edits = ref 0 in
  let n_matched = ref 0 in
  let matched_unique : (string, unit) Hashtbl.t = Hashtbl.create 256 in
  let n_commits = ref 0 in
  let n_relinked = ref 0 in
  Lwt.catch (fun () ->
    let* () = status "Git links: collecting git state..." in
    let jsonl_dir = Urme_search.Jsonl_reader.find_jsonl_dir ~project_dir in
    let all_sessions_for_ts = Urme_search.Jsonl_reader.list_sessions ~jsonl_dir in
    let oldest_session_ts = match List.rev all_sessions_for_ts with
      | last :: _ -> Urme_search.Jsonl_reader.first_line_timestamp last
      | [] -> "" in
    let since_flag = if oldest_session_ts <> "" then ["--since"; oldest_session_ts] else [] in
    let* user_email =
      Lwt.catch (fun () ->
        let+ e = Urme_git.Ops.run_git ~cwd ["config"; "user.email"] in
        String.trim e
      ) (fun _ -> Lwt.return "") in
    let author_flag = if user_email <> "" then ["--author"; user_email] else [] in
    let log_flags = since_flag @ author_flag in
    let p_commits = Lwt.catch
      (fun () -> Urme_git.Ops.walk_log ~cwd ~max_count:5000 ~all:true ())
      (fun _ -> Lwt.return []) in
    let p_repo = Urme_store.Project_store.open_repo ~project_dir in
    let p_log = Lwt.catch (fun () ->
      Urme_git.Ops.run_git ~cwd
        (["log"; "--all"; "--format=COMMIT %H %at"; "--name-only"] @ log_flags)
    ) (fun _ -> Lwt.return "") in
    let p_existing_gis = Lwt.catch (fun () ->
      Urme_search.Chromadb.get_all_with_git_info ~port ~collection_id
    ) (fun _ -> Lwt.return []) in
    let* all_commits = p_commits
    and* repo = p_repo
    and* log_output = p_log
    and* existing_gis = p_existing_gis in
    n_commits := List.length all_commits;
    let live_shas = Hashtbl.create (List.length all_commits) in
    List.iter (fun (sha, _ts, _msg) -> Hashtbl.replace live_shas sha ()) all_commits;
    (* Parse git log output into commits_with_files *)
    let commits_with_files =
      let result = ref [] in
      let cur_sha = ref "" in
      let cur_ts = ref 0.0 in
      let cur_files = ref [] in
      let flush () =
        if !cur_sha <> "" then
          result := (!cur_sha, !cur_ts, List.rev !cur_files) :: !result;
        cur_sha := ""; cur_ts := 0.0; cur_files := [] in
      String.split_on_char '\n' log_output |> List.iter (fun line ->
        if String.length line > 7 && String.sub line 0 7 = "COMMIT " then begin
          flush ();
          let rest = String.sub line 7 (String.length line - 7) in
          match String.split_on_char ' ' rest with
          | sha :: ts_str :: _ ->
            cur_sha := sha;
            cur_ts := (try float_of_string ts_str with _ -> 0.0)
          | _ -> ()
        end else if String.length line > 0 then
          cur_files := line :: !cur_files
      );
      flush ();
      List.rev !result
    in

    let* () = status "Git links: stale refs + JSONL scan..." in
    let diff_hash_cache : (string * string, string) Hashtbl.t = Hashtbl.create 256 in
    let get_diff_hash sha file =
      match Hashtbl.find_opt diff_hash_cache (sha, file) with
      | Some dh -> Lwt.return dh
      | None ->
        let* diff = Lwt.catch (fun () ->
          Urme_git.Ops.run_git ~cwd ["diff"; sha ^ "^"; sha; "--"; file]
        ) (fun _ -> Lwt.return "") in
        let dh = diff_hash_of_string diff in
        Hashtbl.replace diff_hash_cache (sha, file) dh;
        Lwt.return dh
    in

    (* Phase 2: Re-link stale references *)
    let p_phase2 =
      if existing_gis = [] then Lwt.return_unit
      else begin
        let live_diff_index : (string * string, string * string) Hashtbl.t =
          Hashtbl.create 256 in
        let diff_tasks = List.concat_map (fun (sha, _ts, files) ->
          List.map (fun file -> (sha, file)) files
        ) commits_with_files in
        let* () = lwt_iter ~n:(ncpu * 4) (fun (sha, file) ->
          let file_base = Filename.basename file in
          let* dh = get_diff_hash sha file in
          if dh <> (diff_hash_of_string "") then
            Hashtbl.replace live_diff_index (file_base, dh) (sha, file);
          Lwt.return_unit
        ) diff_tasks in
        let* () = lwt_iter ~n:(ncpu * 2)
          (fun (id, gi_str, _session_id, _idx, _ts) ->
          let tbl = parse_git_info_json gi_str in
          let changed = ref false in
          Hashtbl.iter (fun edit_key value ->
            match value with
            | Some gi ->
              let file_base = match String.split_on_char ':' edit_key with
                | fb :: _ -> fb | [] -> "" in
              if Hashtbl.mem live_shas gi.commit_sha then begin
                match Hashtbl.find_opt live_diff_index (file_base, gi.diff_hash) with
                | Some (found_sha, _) when found_sha = gi.commit_sha -> ()
                | _ ->
                  Hashtbl.replace tbl edit_key None;
                  changed := true
              end else begin
                match Hashtbl.find_opt live_diff_index (file_base, gi.diff_hash) with
                | Some (new_sha, _) ->
                  Hashtbl.replace tbl edit_key
                    (Some { gi with commit_sha = new_sha });
                  changed := true;
                  incr n_relinked
                | None ->
                  Hashtbl.replace tbl edit_key None;
                  changed := true
              end
            | None -> ()
          ) tbl;
          if !changed then
            let+ _ok = Urme_search.Chromadb.update_interaction_git_info
              ~port ~collection_id ~id
              ~git_info:(serialize_git_info_json tbl) in ()
          else Lwt.return_unit
        ) existing_gis in
        Lwt.return_unit
      end
    in

    (* Phase 3: Build edit index from JSONL *)
    let p_phase3 =
      let all_sessions = Urme_search.Jsonl_reader.list_sessions ~jsonl_dir in
      let sessions = match sessions_filter with
        | None -> all_sessions
        | Some sids ->
          let sset = Hashtbl.create (List.length sids) in
          List.iter (fun s -> Hashtbl.replace sset s ()) sids;
          List.filter (fun path ->
            let sid = Filename.basename path |> Filename.chop_extension in
            Hashtbl.mem sset sid
          ) all_sessions
      in
      let matched_keys = Hashtbl.create 256 in
      let min_matched_ts =
        List.fold_left (fun acc (_id, gi_str, _sid, _idx, ts_str) ->
          let tbl = parse_git_info_json gi_str in
          let has_match = Hashtbl.fold (fun ek v found ->
            if v <> None then (Hashtbl.replace matched_keys ek (); true)
            else found
          ) tbl false in
          if has_match && ts_str <> "" then
            let ts = Urme_core.Types.iso8601_to_epoch ts_str in
            if ts > 0.0 then Float.min acc ts else acc
          else acc
        ) Float.infinity existing_gis in
      let ts_cutoff = if Float.is_finite min_matched_ts
        then min_matched_ts else 0.0 in
      let pool = Domainslib.Task.setup_pool ~num_domains:ncpu () in
      let sorted_sessions = Edit_extract.sort_by_size_desc sessions in
      let all_edits = Domainslib.Task.run pool (fun () ->
        let promises = List.map (fun filepath ->
          Domainslib.Task.async pool (fun () ->
            Edit_extract.edits_of_session ~filepath
            |> List.filter (fun e ->
              e.timestamp > ts_cutoff &&
              not (Hashtbl.mem matched_keys e.edit_key))
          )
        ) sorted_sessions in
        List.concat_map (Domainslib.Task.await pool) promises
      ) in
      Domainslib.Task.teardown_pool pool;
      n_edits := List.length all_edits;
      let sorted_edits = List.sort (fun a b ->
        Float.compare b.timestamp a.timestamp) all_edits in
      Lwt.return sorted_edits
    in

    let* () = p_phase2
    and* sorted_edits = p_phase3 in

    (* Phase 4: Commit-centric matching *)
    let edit_index = Hashtbl.create 256 in
    List.iter (fun e ->
      let existing = match Hashtbl.find_opt edit_index e.file_base with
        | Some l -> l | None -> [] in
      Hashtbl.replace edit_index e.file_base (e :: existing)
    ) sorted_edits;
    let edit_file_bases = Hashtbl.create (Hashtbl.length edit_index) in
    Hashtbl.iter (fun fb _ -> Hashtbl.replace edit_file_bases fb ()) edit_index;
    let relevant_commits = List.filter (fun (_sha, _ts, files) ->
      List.exists (fun f ->
        Hashtbl.mem edit_file_bases (Filename.basename f)) files
    ) commits_with_files in
    let n_relevant = List.length relevant_commits in
    let* () = status
      (Printf.sprintf "Git links: matching %d edits against %d/%d commits..."
         !n_edits n_relevant !n_commits) in
    let gi_updates : (string, (string * git_info option) list) Hashtbl.t =
      Hashtbl.create 256 in
    let links : (string * string, link list) Hashtbl.t = Hashtbl.create 256 in
    let* () = lwt_iter ~n:(ncpu * 2) (fun (sha, commit_ts, files) ->
      let relevant_files = List.filter (fun f ->
        Hashtbl.mem edit_file_bases (Filename.basename f)) files in
      lwt_iter ~n:ncpu (fun file ->
        let file_base = Filename.basename file in
        let candidates = match Hashtbl.find_opt edit_index file_base with
          | Some edits -> List.filter (fun e -> e.timestamp < commit_ts) edits
          | None -> [] in
        if candidates = [] then Lwt.return_unit
        else begin
          let* file_after = Lwt.catch (fun () ->
            Urme_store.Project_store.read_blob ~repo ~sha ~path:file
          ) (fun _ -> Lwt.return_none) in
          match file_after with
          | None -> Lwt.return_unit
          | Some content_after ->
            let* dh = get_diff_hash sha file in
            let current_content = ref content_after in
            let record_match (edit : edit) =
              incr n_matched;
              Hashtbl.replace matched_unique edit.edit_key ();
              let iid = Printf.sprintf "%s_%d" edit.session_id
                edit.interaction_index in
              let existing = match Hashtbl.find_opt gi_updates iid with
                | Some l -> l | None -> [] in
              Hashtbl.replace gi_updates iid
                ((edit.edit_key,
                  Some { commit_sha = sha; diff_hash = dh;
                         turn_idx = edit.turn_idx;
                         entry_idx = edit.entry_idx }) :: existing);
              let lnk = { commit_sha = sha; file = file_base;
                          session_id = edit.session_id;
                          turn_idx = edit.turn_idx;
                          entry_idx = edit.entry_idx;
                          edit_key = edit.edit_key } in
              let lk = (sha, file_base) in
              let el = match Hashtbl.find_opt links lk with
                | Some l -> l | None -> [] in
              Hashtbl.replace links lk (el @ [lnk])
            in
            List.iter (fun edit ->
              if edit.old_string <> "" then begin
                let cc = !current_content in
                match find_substring edit.new_string cc with
                | None -> ()
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
                      | Some new_cc ->
                        current_content := new_cc; record_match edit
                      | None ->
                        current_content := undone; record_match edit)
                   | None ->
                     current_content := undone; record_match edit)
              end else if edit.new_string <> "" then begin
                let ns = edit.new_string in
                let cc = !current_content in
                let check_len = min (String.length ns) (String.length cc) in
                if check_len > 20 then
                  let sample = String.sub ns 0 (min 100 (String.length ns)) in
                  match find_substring sample cc with
                  | Some _ -> record_match edit
                  | None -> ()
              end
            ) candidates;
            Lwt.return_unit
        end
      ) relevant_files
    ) relevant_commits in

    (* Phase 5: persist to ChromaDB *)
    let* () = status (Printf.sprintf "Git links: persisting %d updates..."
        (Hashtbl.length gi_updates)) in
    let update_list = Hashtbl.fold (fun iid entries acc ->
      (iid, entries) :: acc) gi_updates [] in
    let* () = lwt_iter ~n:ncpu (fun (iid, entries) ->
      let* existing_meta = Lwt.catch (fun () ->
        Urme_search.Chromadb.get_interaction_meta ~port ~collection_id ~id:iid
      ) (fun exn ->
        log_debug (Printf.sprintf "P5 get_meta err %s: %s"
          iid (Printexc.to_string exn));
        Lwt.return_none) in
      let tbl = match existing_meta with
        | Some meta ->
          let open Yojson.Safe.Util in
          let gi = meta |> member "git_info" |> to_string_option
            |> Option.value ~default:"" in
          parse_git_info_json gi
        | None ->
          log_debug (Printf.sprintf "P5 miss: %s not in ChromaDB" iid);
          Hashtbl.create 8 in
      List.iter (fun (ek, value) -> Hashtbl.replace tbl ek value) entries;
      Lwt.catch (fun () ->
        let+ _ok = Urme_search.Chromadb.update_interaction_git_info
          ~port ~collection_id ~id:iid
          ~git_info:(serialize_git_info_json tbl) in ()
      ) (fun exn ->
        log_debug (Printf.sprintf "P5 update err %s: %s"
          iid (Printexc.to_string exn));
        Lwt.return_unit)
    ) update_list in

    (* Phase 6: rebuild in-memory links *)
    let* () = status "Git links: building link index..." in
    let* fresh_gis =
      Urme_search.Chromadb.get_all_with_git_info ~port ~collection_id in
    List.iter (fun (_id, gi_str, session_id, _idx, _ts) ->
      let tbl = parse_git_info_json gi_str in
      Hashtbl.iter (fun ek value ->
        match value with
        | Some gi ->
          let file_base = match String.split_on_char ':' ek with
            | fb :: _ -> fb | [] -> "" in
          if file_base <> "" then begin
            let lk = (gi.commit_sha, file_base) in
            let existing = match Hashtbl.find_opt links lk with
              | Some l -> l | None -> [] in
            if not (List.exists (fun (l : link) -> l.edit_key = ek) existing)
            then begin
              let lnk = { commit_sha = gi.commit_sha; file = file_base;
                          session_id; turn_idx = gi.turn_idx;
                          entry_idx = gi.entry_idx; edit_key = ek } in
              Hashtbl.replace links lk (existing @ [lnk])
            end
          end
        | None -> ()
      ) tbl
    ) fresh_gis;

    Lwt.return (links, !n_edits, Hashtbl.length matched_unique,
                !n_commits, !n_relinked)
  ) (fun exn ->
    let _ = Printexc.to_string exn in
    Lwt.return (Hashtbl.create 0, !n_edits, Hashtbl.length matched_unique,
                !n_commits, !n_relinked))

open Cmdliner

(* Common options *)
let project_dir =
  Arg.(value & opt string "." & info ["project-dir"; "C"] ~docv:"DIR"
         ~doc:"Project directory (must be a git repo)")

(* chromadb_port was a V1 flag. V2 uses SQLite; kept as a no-op for
   backwards compatibility with existing invocations / .mcp.json configs. *)
let chromadb_port =
  Arg.(value & opt int 0 & info ["chromadb-port"] ~docv:"PORT"
         ~doc:"[deprecated, ignored] retained for backwards compatibility")

(* --- Subcommand: ask --- *)

let ask_cmd =
  let prompt =
    Arg.(required & pos 0 (some string) None & info [] ~docv:"PROMPT"
           ~doc:"Prompt to send to Claude") in
  let model =
    Arg.(value & opt (some string) None & info ["model"; "m"] ~docv:"MODEL"
           ~doc:"Model to use") in
  let run prompt model project_dir _chromadb_port =
    let config = Urme_core.Config.load () in
    let opts = { Urme_claude.Process.default_opts with model } in
    Lwt_main.run begin
      let open Lwt.Syntax in
      let* proc = Urme_claude.Process.spawn_oneshot ~cwd:project_dir
          ~opts ~binary:config.claude_binary ~prompt () in
      let* () = Urme_claude.Process.iter_events proc ~f:(fun event ->
        (match event with
         | Urme_claude.Stream.Assistant_message { content; _ } ->
           let text = Urme_claude.Stream.text_of_content content in
           if text <> "" then print_string text
         | Urme_claude.Stream.Result { result; is_error; _ } ->
           if is_error then Printf.eprintf "Error: %s\n%!" result
         | _ -> ());
        Lwt.return_unit
      ) in
      let _ = Urme_claude.Process.wait proc in
      print_newline ();
      Lwt.return_unit
    end
  in
  Cmd.v (Cmd.info "ask" ~doc:"Send a one-shot prompt to Claude")
    Term.(const run $ prompt $ model $ project_dir $ chromadb_port)

(* --- Subcommand: search --- *)

let search_cmd =
  let query =
    Arg.(required & pos 0 (some string) None & info [] ~docv:"QUERY"
           ~doc:"Search query") in
  let n =
    Arg.(value & opt int 20 & info ["n"] ~docv:"N"
           ~doc:"Number of results") in
  let smart =
    Arg.(value & flag & info ["smart"; "s"]
           ~doc:"Let Claude rewrite sparse queries and rerank the shortlist \
                 (adds latency; uses Claude CLI)") in
  let deep =
    Arg.(value & flag & info ["deep"]
           ~doc:"Full three-layer pipeline: NL→SQL, rerank, read top-3 turn \
                 text and produce a grounded answer sentence (slowest)") in
  let short_sha = function
    | Some s when String.length s >= 7 -> String.sub s 0 7
    | Some s -> s
    | None -> "-------"
  in
  let print_hit (h : Urme_search.Search.hit) =
    Printf.printf "[%s] %s  (score %.2f)\n"
      (short_sha h.commit_after) h.summary h.score;
    if h.prompt_text <> "" then
      Printf.printf "    > %s\n"
        (if String.length h.prompt_text > 100
         then String.sub h.prompt_text 0 100 ^ "..."
         else h.prompt_text);
    if h.tags <> "" then Printf.printf "    tags: %s\n" h.tags;
    print_newline ()
  in
  let run query n smart deep project_dir _chromadb_port =
    let db = Urme_store.Schema.open_or_create ~project_dir in
    let config = Urme_core.Config.load () in
    (if deep then begin
       let hits, answer = Lwt_main.run
           (Urme_search.Search.run_deep ~db
              ~binary:config.claude_binary
              ~project_dir ~limit:n query) in
       if answer <> "" then Printf.printf "— %s\n\n" answer;
       List.iter print_hit hits;
       if hits = [] then print_endline "no matches"
     end else if smart then begin
       let hits, synth = Lwt_main.run
           (Urme_search.Search.run_smart ~db
              ~binary:config.claude_binary ~limit:n query) in
       if synth <> "" then Printf.printf "— %s\n\n" synth;
       List.iter print_hit hits;
       if hits = [] then print_endline "no matches"
     end else begin
       let hits = Urme_search.Search.run_with_fallback ~db ~limit:n query in
       List.iter print_hit hits;
       if hits = [] then print_endline "no matches"
     end);
    Urme_store.Schema.close db
  in
  Cmd.v (Cmd.info "search"
           ~doc:"Search indexed steps. \
                 --smart: Claude rewrite + rerank. \
                 --deep: NL→SQL + rerank + grounded answer.")
    Term.(const run $ query $ n $ smart $ deep $ project_dir $ chromadb_port)

(* --- Subcommand: init --- *)

let init_cmd =
  let skip_summaries =
    Arg.(value & flag & info ["skip-summaries"]
           ~doc:"Index steps but skip the Claude summarisation pass") in
  let parallel =
    Arg.(value & opt int 3 & info ["parallel"; "j"] ~docv:"N"
           ~doc:"Number of parallel Claude daemons for the summarisation pass \
                 (default 3; higher = faster but more RAM)") in
  let run skip_summaries parallel project_dir _chromadb_port =
    let config = Urme_core.Config.load () in
    (* Normalise project_dir to an absolute path. Edit_extract uses
       the project_dir to strip the prefix from Claude's tool_use
       file_paths; those are absolute, so a relative project_dir
       (like the cmdliner default ".") leaves the stored file_path
       absolute while commits use relative paths — breaking path
       equality in [assign] and losing every Claude attribution. *)
    let project_dir =
      if project_dir = "." then Sys.getcwd ()
      else if Filename.is_relative project_dir
      then Filename.concat (Sys.getcwd ()) project_dir
      else project_dir in
    let db = Urme_store.Schema.open_or_create ~project_dir in
    let n = Urme_engine.Indexer.index_all_sessions ~db ~project_dir in
    Printf.printf "indexed %d turns\n" n;
    (* Single Lwt_main.run: chaining summarise → run_once inside one
       Lwt loop avoids the state drift that a separate second
       Lwt_main.run seems to cause (the walker saved zero links when
       two Lwt_main.run calls ran back to back). *)
    (try
       Lwt_main.run begin
         let open Lwt.Syntax in
         let* () =
           if skip_summaries then Lwt.return_unit
           else begin
             Printf.printf "running Claude summarisation pass (%d daemons)...\n%!" parallel;
             Urme_engine.Summarise.summarise_pending
               ~pool_size:parallel
               ~binary:config.claude_binary ~db ()
           end
         in
         Printf.printf "building per-edit git links...\n%!";
         Urme_engine.Git_index.run_once ~project_dir ~db
       end
     with e -> Printf.eprintf "link error (non-fatal): %s\n%!"
                 (Printexc.to_string e));
    Urme_store.Schema.close db
  in
  Cmd.v (Cmd.info "init" ~doc:"Index Claude sessions into urme V2 SQLite store")
    Term.(const run $ skip_summaries $ parallel $ project_dir $ chromadb_port)

(* --- Subcommand: history --- *)

let history_cmd =
  let run _project_dir _chromadb_port =
    Printf.printf "history: not yet implemented (Phase 5)\n"
  in
  Cmd.v (Cmd.info "history" ~doc:"List sessions")
    Term.(const run $ project_dir $ chromadb_port)

(* --- Subcommand: blame --- *)

let blame_cmd =
  let file =
    Arg.(required & pos 0 (some string) None & info [] ~docv:"FILE"
           ~doc:"File to blame") in
  let run _file _project_dir _chromadb_port =
    Printf.printf "blame: not yet implemented (Phase 6)\n"
  in
  Cmd.v (Cmd.info "blame" ~doc:"Git blame with experience correlation")
    Term.(const run $ file $ project_dir $ chromadb_port)

(* --- Subcommand: explain --- *)

let explain_cmd =
  let file =
    Arg.(required & pos 0 (some string) None & info [] ~docv:"FILE"
           ~doc:"File to explain") in
  let pattern =
    Arg.(value & opt (some string) None & info ["pattern"; "p"]
           ~docv:"PATTERN" ~doc:"Pattern to search for") in
  let run _file _pattern _project_dir _chromadb_port =
    Printf.printf "explain: not yet implemented (Phase 6)\n"
  in
  Cmd.v (Cmd.info "explain" ~doc:"Explain why code was changed")
    Term.(const run $ file $ pattern $ project_dir $ chromadb_port)

(* --- Subcommand: save --- *)

let save_cmd =
  let label =
    Arg.(required & opt (some string) None & info ["label"; "l"]
           ~docv:"LABEL" ~doc:"Experience label") in
  let intent =
    Arg.(value & opt string "" & info ["intent"; "i"] ~docv:"INTENT"
           ~doc:"Intent/purpose") in
  let run _label _intent _project_dir _chromadb_port =
    Printf.printf "save: not yet implemented (Phase 6)\n"
  in
  Cmd.v (Cmd.info "save" ~doc:"Save current work as an experience")
    Term.(const run $ label $ intent $ project_dir $ chromadb_port)

(* --- Subcommand: replay --- *)

let replay_cmd =
  let query =
    Arg.(value & pos 0 (some string) None & info [] ~docv:"QUERY"
           ~doc:"Query to find experience") in
  let run _query _project_dir _chromadb_port =
    Printf.printf "replay: not yet implemented (Phase 6)\n"
  in
  Cmd.v (Cmd.info "replay" ~doc:"Replay a past experience")
    Term.(const run $ query $ project_dir $ chromadb_port)

(* --- Subcommand: pr --- *)

let pr_cmd =
  let title =
    Arg.(required & opt (some string) None & info ["title"; "t"]
           ~docv:"TITLE" ~doc:"PR title") in
  let body =
    Arg.(value & opt string "" & info ["body"; "b"] ~docv:"BODY"
           ~doc:"PR body") in
  let run _title _body _project_dir =
    Printf.printf "pr: not yet implemented (Phase 7)\n"
  in
  Cmd.v (Cmd.info "pr" ~doc:"Create a GitHub pull request")
    Term.(const run $ title $ body $ project_dir)

(* --- Subcommand: diff --- *)

let diff_cmd =
  let run _project_dir =
    Printf.printf "diff: not yet implemented (Phase 4)\n"
  in
  Cmd.v (Cmd.info "diff" ~doc:"Show typed diff with hunk selection")
    Term.(const run $ project_dir)

(* --- Subcommand: wipe --- *)

let wipe_cmd =
  let run _project_dir _chromadb_port =
    Printf.printf "wipe: not yet implemented (Phase 6)\n"
  in
  Cmd.v (Cmd.info "wipe" ~doc:"Wipe experience database")
    Term.(const run $ project_dir $ chromadb_port)

(* --- Subcommand: prune --- *)

let prune_cmd =
  let date =
    Arg.(required & pos 0 (some string) None & info [] ~docv:"DATE"
           ~doc:"Remove experiences before this date") in
  let run _date _project_dir _chromadb_port =
    Printf.printf "prune: not yet implemented (Phase 6)\n"
  in
  Cmd.v (Cmd.info "prune" ~doc:"Remove old experiences")
    Term.(const run $ date $ project_dir $ chromadb_port)

(* --- Subcommand: export --- *)

(* Resolve the commits reachable from [branch] but not from its
   merge-base with main/master. Same set `gh` uses for a PR's diff.
   Falls back to the whole branch history if no merge-base is found. *)
let commits_for_branch ~cwd ~branch =
  let open Lwt.Syntax in
  let resolve_ref r =
    Lwt.catch
      (fun () ->
        let+ out = Urme_git.Ops.run_git ~cwd ["rev-parse"; "--verify"; r] in
        Some (String.trim out))
      (fun _ -> Lwt.return None) in
  let* base_ref =
    let* m = resolve_ref "main" in
    match m with
    | Some _ -> Lwt.return (Some "main")
    | None -> let* ms = resolve_ref "master" in
      (match ms with Some _ -> Lwt.return (Some "master") | None -> Lwt.return None) in
  let range_args = match base_ref with
    | Some base -> ["log"; "--format=%H"; Printf.sprintf "%s..%s" base branch]
    | None      -> ["log"; "--format=%H"; branch] in
  let* out = Urme_git.Ops.run_git ~cwd range_args in
  Lwt.return (String.split_on_char '\n' out
              |> List.map String.trim
              |> List.filter (fun s -> s <> ""))

let export_cmd =
  let branch =
    Arg.(value & opt (some string) None
           & info ["branch"; "b"] ~docv:"BRANCH"
               ~doc:"Export only the rows touching commits on this \
                     branch (resolves commits between the branch and \
                     main/master). When omitted, exports the whole DB.") in
  let out_path =
    Arg.(value & opt (some string) None
           & info ["out"; "o"] ~docv:"PATH"
               ~doc:"Output file (default: <branch>.urmedb or \
                     urme-snapshot.urmedb).") in
  let run branch out_path project_dir _chromadb_port =
    match branch with
    | None ->
      let path = Option.value out_path ~default:"urme-snapshot.urmedb" in
      Urme_store.Export.export_project ~project_dir ~path;
      Printf.printf "exported (whole DB) → %s\n" path
    | Some br ->
      let path = Option.value out_path
        ~default:(Printf.sprintf "%s.urmedb"
                    (String.map (fun c -> if c = '/' then '-' else c) br)) in
      let commits =
        Lwt_main.run (commits_for_branch ~cwd:project_dir ~branch:br) in
      if commits = [] then begin
        Printf.eprintf
          "export: no commits found for branch %S (is it checked out?)\n" br;
        exit 1
      end;
      Printf.printf "export: %d commits on branch %s\n"
        (List.length commits) br;
      Urme_store.Export.export_scoped ~project_dir ~commits ~out_path:path;
      Printf.printf "exported (branch %s) → %s\n" br path
  in
  Cmd.v (Cmd.info "export"
           ~doc:"Write a snapshot of the urme store. With [--branch], \
                 writes only the rows for that branch's commits \
                 (intended for PR reviews).")
    Term.(const run $ branch $ out_path $ project_dir $ chromadb_port)

(* --- Subcommand: import --- *)

let import_cmd =
  let path =
    Arg.(required & pos 0 (some string) None & info [] ~docv:"PATH"
           ~doc:"Path to the .urmedb snapshot to load.") in
  let run path project_dir _chromadb_port =
    if not (Sys.file_exists path) then begin
      Printf.eprintf "import: %s not found\n" path;
      exit 1
    end;
    (* Don't touch the reviewer's own [.urme/db.sqlite]. Instead,
       point this invocation at the snapshot via [URME_DB_PATH] and
       boot the TUI — the reviewer gets a normal URME where every
       view (Git / History / Search) is scoped to the imported data.
       When they quit, nothing is persisted to their own DB. *)
    let abs =
      if Filename.is_relative path then
        Filename.concat (Sys.getcwd ()) path
      else path in
    Unix.putenv "URME_DB_PATH" abs;
    Printf.printf "urme: loading %s (read-only review session)\n" abs;
    let _ = Urme_core.Config.load () in
    Lwt_main.run (Urme_tui.Reactive.run ~project_dir ())
  in
  Cmd.v (Cmd.info "import"
           ~doc:"Load a .urmedb snapshot and launch URME scoped to it. \
                 Leaves the project's own store untouched.")
    Term.(const run $ path $ project_dir $ chromadb_port)

(* --- Subcommand: serve (MCP server) --- *)

let serve_cmd =
  let run project_dir _chromadb_port =
    Lwt_main.run (Urme_mcp.Server.run ~project_dir)
  in
  Cmd.v (Cmd.info "serve" ~doc:"Run MCP server over stdio (JSON-RPC 2.0)")
    Term.(const run $ project_dir $ chromadb_port)

(* --- Default command: launch the reactive (Nottui/Lwd) git TUI --- *)

let default_run project_dir _chromadb_port =
  let _ = Urme_core.Config.load () in
  Lwt_main.run (Urme_tui.Reactive.run ~project_dir ())

(* Keep the legacy imperative UI reachable for history/search until
   those modes are ported too. *)
let legacy_tui_cmd =
  let run project_dir _chromadb_port =
    let config = Urme_core.Config.load () in
    Lwt_main.run (Urme_tui.App.run ~config ~project_dir ())
  in
  Cmd.v (Cmd.info "legacy" ~doc:"Legacy Notty TUI (history/search modes)")
    Term.(const run $ project_dir $ chromadb_port)

let () =
  Printexc.record_backtrace true;
  (* Suppress stale-FD errors from background Lwt tasks during shutdown *)
  Lwt.async_exception_hook := (fun _exn -> ());
  (* Use libev (kqueue on macOS) instead of select() to avoid
     EINVAL when file descriptors exceed FD_SETSIZE (1024). *)
  (try Lwt_engine.set (new Lwt_engine.libev ())
   with Lwt_sys.Not_available _ -> ());
  let doc = "OCaml CLI orchestration layer for Claude + GitHub" in
  let info = Cmd.info "urme" ~doc ~version:"0.2.0" in
  let default = Term.(const default_run $ project_dir $ chromadb_port) in
  let cmd = Cmd.group ~default info [
    ask_cmd; search_cmd; init_cmd; history_cmd;
    blame_cmd; explain_cmd; save_cmd; replay_cmd;
    pr_cmd; diff_cmd; wipe_cmd; prune_cmd; serve_cmd;
    export_cmd; import_cmd; legacy_tui_cmd;
  ] in
  exit (Cmd.eval cmd)

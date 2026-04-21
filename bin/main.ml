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
  let run query n smart project_dir _chromadb_port =
    let db = Urme_store.Schema.open_or_create ~project_dir in
    if smart then begin
      let config = Urme_core.Config.load () in
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
    end;
    Urme_store.Schema.close db
  in
  Cmd.v (Cmd.info "search" ~doc:"Search indexed steps by FTS5 (with --smart: Claude rewrite + rerank)")
    Term.(const run $ query $ n $ smart $ project_dir $ chromadb_port)

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
    let db = Urme_store.Schema.open_or_create ~project_dir in
    let n = Urme_engine.Indexer.index_all_sessions ~db ~project_dir in
    Printf.printf "indexed %d turns\n" n;
    (* Summarisation runs FIRST: it spawns the claude CLI via
       Unix.fork, which OCaml 5 forbids after any Domain.spawn. The
       git linker below uses Domainslib, so we must summarise before
       touching any multicore code. *)
    if not skip_summaries then begin
      Printf.printf "running Claude summarisation pass (Haiku 4.5, %d daemons)...\n%!"
        parallel;
      Lwt_main.run (Urme_engine.Summarise.summarise_pending
                      ~pool_size:parallel
                      ~binary:config.claude_binary ~db ())
    end;
    Printf.printf "building per-edit git links...\n%!";
    (try Lwt_main.run (
       let pool = Domainslib.Task.setup_pool ~num_domains:4 () in
       let edits = Urme_engine.Edit_extract.edits_of_sessions
           ~pool ~project_dir in
       Domainslib.Task.teardown_pool pool;
       Urme_engine.Git_index.update ~project_dir ~db ~edits)
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

let export_cmd =
  let path =
    Arg.(required & pos 0 (some string) None & info [] ~docv:"PATH"
           ~doc:"Destination file for the SQLite snapshot") in
  let run path project_dir _chromadb_port =
    Urme_store.Export.export_project ~project_dir ~path;
    Printf.printf "exported %s\n" path
  in
  Cmd.v (Cmd.info "export" ~doc:"Write a snapshot of the V2 SQLite store")
    Term.(const run $ path $ project_dir $ chromadb_port)

(* --- Subcommand: import --- *)

let import_cmd =
  let path =
    Arg.(required & pos 0 (some string) None & info [] ~docv:"PATH"
           ~doc:"SQLite snapshot to restore") in
  let force =
    Arg.(value & flag & info ["force"; "f"]
           ~doc:"Overwrite an existing store at .urme/db.sqlite") in
  let run path force project_dir _chromadb_port =
    Urme_store.Export.import_project ~project_dir ~path ~force ();
    Printf.printf "imported %s\n" path
  in
  Cmd.v (Cmd.info "import" ~doc:"Restore a V2 SQLite snapshot into this project")
    Term.(const run $ path $ force $ project_dir $ chromadb_port)

(* --- Subcommand: serve (MCP server) --- *)

let serve_cmd =
  let run project_dir _chromadb_port =
    Lwt_main.run (Urme_mcp.Server.run ~project_dir)
  in
  Cmd.v (Cmd.info "serve" ~doc:"Run MCP server over stdio (JSON-RPC 2.0)")
    Term.(const run $ project_dir $ chromadb_port)

(* --- Default command: launch TUI --- *)

let default_run project_dir _chromadb_port =
  let config = Urme_core.Config.load () in
  Lwt_main.run (Urme_tui.App.run ~config ~project_dir ())

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
    export_cmd; import_cmd;
  ] in
  exit (Cmd.eval cmd)

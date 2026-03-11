open Cmdliner

(* Common options *)
let project_dir =
  Arg.(value & opt string "." & info ["project-dir"; "C"] ~docv:"DIR"
         ~doc:"Project directory (must be a git repo)")

let chromadb_port =
  Arg.(value & opt int 8000 & info ["chromadb-port"] ~docv:"PORT"
         ~doc:"ChromaDB port")

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
    Arg.(value & opt int 5 & info ["n"] ~docv:"N"
           ~doc:"Number of results") in
  let run _query _n _project_dir _chromadb_port =
    Printf.printf "search: not yet implemented (Phase 6)\n"
  in
  Cmd.v (Cmd.info "search" ~doc:"Search experiences")
    Term.(const run $ query $ n $ project_dir $ chromadb_port)

(* --- Subcommand: init --- *)

let init_cmd =
  let since =
    Arg.(value & opt string "" & info ["since"] ~docv:"DATE"
           ~doc:"Start date (ISO format)") in
  let run _since _project_dir _chromadb_port =
    Printf.printf "init: not yet implemented (Phase 6)\n"
  in
  Cmd.v (Cmd.info "init" ~doc:"Bootstrap experience DB from git history")
    Term.(const run $ since $ project_dir $ chromadb_port)

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

(* --- Default command: launch TUI --- *)

let default_run project_dir _chromadb_port =
  let config = Urme_core.Config.load () in
  Lwt_main.run (Urme_tui.App.run ~config ~project_dir ())

let () =
  Printexc.record_backtrace true;
  let doc = "OCaml CLI orchestration layer for Claude + GitHub" in
  let info = Cmd.info "urme" ~doc ~version:"0.1.0" in
  let default = Term.(const default_run $ project_dir $ chromadb_port) in
  let cmd = Cmd.group ~default info [
    ask_cmd; search_cmd; init_cmd; history_cmd;
    blame_cmd; explain_cmd; save_cmd; replay_cmd;
    pr_cmd; diff_cmd; wipe_cmd; prune_cmd;
  ] in
  exit (Cmd.eval cmd)

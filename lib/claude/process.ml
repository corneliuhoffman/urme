(* Claude CLI subprocess — oneshot + persistent daemon modes.

   Both are spawned via [Unix.create_process_env] (posix_spawn on
   supported platforms) so they remain safe after [Domain.spawn].

   - [spawn_oneshot] : one prompt, fresh process, closes on completion.
     Good for ad-hoc calls where the ~10s claude startup tax is fine.
   - [spawn_daemon]  : long-lived process, accepts N prompts over stdin
     as NDJSON, emits NDJSON events on stdout. Good for high-volume
     batched work (e.g. summarising hundreds of turns) — the startup
     tax is paid once. *)

open Lwt.Syntax

type claude_opts = {
  model          : string option;
  system_prompt  : string option;
  allowed_tools  : string list;
  max_turns      : int option;
  extra_args     : string list;
  bare           : bool;
    (* --bare skips hooks, LSP, plugins, attribution, auto-memory, keychain
       reads and CLAUDE.md discovery. Cuts the default system prompt from
       ~32K tokens to ~1K. Essential for throughput on summarise-style
       calls where we don't need any of that context. *)
  no_tools       : bool;
    (* --tools "" — disable all tool access. Trims cache further. *)
}

let default_opts = {
  model = None;
  system_prompt = None;
  allowed_tools = [];
  max_turns = None;
  extra_args = [];
  bare = false;
  no_tools = false;
}

(* Strip ANTHROPIC_API_KEY (force Max subscription) and CLAUDECODE. *)
let clean_env () =
  Unix.environment ()
  |> Array.to_list
  |> List.filter (fun s ->
    let starts_with prefix =
      String.length s >= String.length prefix &&
      String.sub s 0 (String.length prefix) = prefix in
    not (starts_with "ANTHROPIC_API_KEY=") &&
    not (starts_with "CLAUDECODE=") &&
    not (starts_with "CLAUDE_CODE_ENTRYPOINT="))
  |> Array.of_list

(* Shared: consume stderr so the child doesn't block on a full pipe.
   Assumes [err_rd] has already been put into non-blocking mode. *)
let drain_stderr err_rd =
  let ic =
    Lwt_io.of_fd ~mode:Lwt_io.input
      (Lwt_unix.of_unix_file_descr ~blocking:false err_rd) in
  Lwt.async (fun () ->
    Lwt.catch (fun () ->
      Lwt_stream.iter (fun _ -> ()) (Lwt_io.read_lines ic)
    ) (fun _ -> Lwt.return_unit))

(* ---------- Oneshot ---------- *)

type oneshot = {
  oneshot_events : Stream.stream_event Lwt_stream.t;
  oneshot_pid    : int;
}

let common_flags ~opts =
  (* --no-session-persistence: don't write session JSONL to
     ~/.claude/projects/…  Critical for our summariser — otherwise
     every `urme init` re-indexes its own prior summarise prompts, and
     each summariser pass recursively summarises summaries. *)
  let args = ["--no-session-persistence"] in
  let args = if opts.bare then args @ ["--bare"] else args in
  let args = if opts.no_tools then args @ ["--tools"; ""] else args in
  let args = match opts.model with
    | Some m -> args @ ["--model"; m] | None -> args in
  let args = match opts.system_prompt with
    | Some s -> args @ ["--system-prompt"; s] | None -> args in
  let args = match opts.max_turns with
    | Some n -> args @ ["--max-turns"; string_of_int n] | None -> args in
  let args = List.fold_left (fun acc tool ->
    acc @ ["--allowedTools"; tool]) args opts.allowed_tools in
  args @ opts.extra_args

let oneshot_args ~opts ~prompt =
  ["--print"; "--output-format"; "stream-json"; "--verbose";
   "-p"; prompt] @ common_flags ~opts

let spawn_oneshot ?(cwd=".") ?(opts=default_opts) ~binary ~prompt () =
  let flags = oneshot_args ~opts ~prompt in
  let argv = Array.of_list (binary :: flags) in
  let env = clean_env () in

  let devnull_in = Unix.openfile "/dev/null" [Unix.O_RDONLY] 0 in
  let out_rd, out_wr = Unix.pipe ~cloexec:true () in
  let err_rd, err_wr = Unix.pipe ~cloexec:true () in

  let pid =
    if cwd = "." || cwd = "" then
      Unix.create_process_env binary argv env devnull_in out_wr err_wr
    else
      let quoted = Array.to_list argv
        |> List.map Filename.quote |> String.concat " " in
      let shell_cmd = Printf.sprintf "cd %s && exec %s"
        (Filename.quote cwd) quoted in
      Unix.create_process_env "/bin/sh"
        [| "/bin/sh"; "-c"; shell_cmd |] env
        devnull_in out_wr err_wr
  in
  Unix.close out_wr; Unix.close err_wr; Unix.close devnull_in;

  Unix.set_nonblock out_rd;
  Unix.set_nonblock err_rd;

  let out_ic =
    Lwt_io.of_fd ~mode:Lwt_io.input
      (Lwt_unix.of_unix_file_descr ~blocking:false out_rd) in
  drain_stderr err_rd;

  let events, push = Lwt_stream.create () in
  let rec reader () =
    let* line_opt = Lwt.catch
      (fun () -> Lwt_io.read_line_opt out_ic)
      (fun _ -> Lwt.return_none) in
    match line_opt with
    | Some line ->
      (match Stream.parse_line line with
       | Some event -> push (Some event)
       | None -> ());
      reader ()
    | None -> push None; Lwt.return_unit
  in
  Lwt.async reader;
  Lwt.return { oneshot_events = events; oneshot_pid = pid }

let iter_events o ~f = Lwt_stream.iter_s f o.oneshot_events

(* EINTR-safe waitpid: signal-driven TUIs (notty, etc.) can interrupt. *)
let rec waitpid_noeintr pid =
  Lwt.catch
    (fun () -> Lwt_unix.waitpid [] pid)
    (function
      | Unix.Unix_error (Unix.EINTR, _, _) -> waitpid_noeintr pid
      | e -> Lwt.fail e)

let wait o =
  let* _pid, status = waitpid_noeintr o.oneshot_pid in
  Lwt.return status

(* ---------- Persistent daemon ---------- *)

type daemon = {
  daemon_pid     : int;
  daemon_stdin   : Lwt_io.output_channel;
  daemon_events  : Stream.stream_event Lwt_stream.t;
}

let daemon_args ~opts =
  ["--print"; "--input-format"; "stream-json";
   "--output-format"; "stream-json"; "--verbose"]
  @ common_flags ~opts

let spawn_daemon ?(cwd=".") ?(opts=default_opts) ~binary () =
  let flags = daemon_args ~opts in
  let argv = Array.of_list (binary :: flags) in
  let env = clean_env () in

  let in_rd, in_wr = Unix.pipe ~cloexec:true () in
  let out_rd, out_wr = Unix.pipe ~cloexec:true () in
  let err_rd, err_wr = Unix.pipe ~cloexec:true () in

  let pid =
    if cwd = "." || cwd = "" then
      Unix.create_process_env binary argv env in_rd out_wr err_wr
    else
      let quoted = Array.to_list argv
        |> List.map Filename.quote |> String.concat " " in
      let shell_cmd = Printf.sprintf "cd %s && exec %s"
        (Filename.quote cwd) quoted in
      Unix.create_process_env "/bin/sh"
        [| "/bin/sh"; "-c"; shell_cmd |] env
        in_rd out_wr err_wr
  in
  Unix.close in_rd; Unix.close out_wr; Unix.close err_wr;

  (* Force non-blocking on the parent's ends so Lwt's scheduler drives
     them via kqueue/epoll instead of dispatching to a background thread
     pool that can starve. *)
  Unix.set_nonblock in_wr;
  Unix.set_nonblock out_rd;
  Unix.set_nonblock err_rd;

  let stdin_chan =
    Lwt_io.of_fd ~mode:Lwt_io.output
      (Lwt_unix.of_unix_file_descr ~blocking:false in_wr) in
  let out_ic =
    Lwt_io.of_fd ~mode:Lwt_io.input
      (Lwt_unix.of_unix_file_descr ~blocking:false out_rd) in
  drain_stderr err_rd;

  let events, push = Lwt_stream.create () in
  let rec reader () =
    let* line_opt = Lwt.catch
      (fun () -> Lwt_io.read_line_opt out_ic)
      (fun _ -> Lwt.return_none) in
    match line_opt with
    | Some line ->
      (match Stream.parse_line line with
       | Some event -> push (Some event)
       | None -> ());
      reader ()
    | None -> push None; Lwt.return_unit
  in
  Lwt.async reader;
  Lwt.return { daemon_pid = pid; daemon_stdin = stdin_chan;
               daemon_events = events }

(* Send one prompt down the daemon's stdin, read events until the next
   [Result] marker, return the accumulated assistant text.

   Assumes the caller serialises ask calls on a single daemon — two
   concurrent asks would interleave event streams and garbage the
   output boundary. The pool in [prompts.ml] enforces this. *)
let ask_daemon d ~prompt =
  let line =
    let json = `Assoc [
      "type", `String "user";
      "message", `Assoc [
        "role", `String "user";
        "content", `String prompt;
      ];
    ] in
    Yojson.Safe.to_string json ^ "\n"
  in
  let* () = Lwt_io.write d.daemon_stdin line in
  let* () = Lwt_io.flush d.daemon_stdin in
  let buf = Buffer.create 1024 in
  let rec loop () =
    let* ev = Lwt_stream.get d.daemon_events in
    match ev with
    | Some (Stream.Assistant_message { content; _ }) ->
      Buffer.add_string buf (Stream.text_of_content content);
      loop ()
    | Some (Stream.Result _) -> Lwt.return (Buffer.contents buf)
    | Some _ -> loop ()
    | None -> Lwt.return (Buffer.contents buf)
  in
  loop ()

let close_daemon d =
  let* () = Lwt.catch (fun () -> Lwt_io.close d.daemon_stdin)
    (fun _ -> Lwt.return_unit) in
  let* _pid, status = waitpid_noeintr d.daemon_pid in
  Lwt.return status

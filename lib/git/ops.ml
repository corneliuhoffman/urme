open Lwt.Syntax

(* EINTR-safe waitpid. Inside the TUI, Notty's signal handlers (SIGWINCH
   etc.) can interrupt waitpid; we just retry. *)
let rec waitpid_noeintr pid =
  Lwt.catch
    (fun () -> Lwt_unix.waitpid [] pid)
    (function
      | Unix.Unix_error (Unix.EINTR, _, _) -> waitpid_noeintr pid
      | e -> Lwt.fail e)

(* Spawn a process, capture stdout, suppress stderr.
   Uses Unix.create_process (posix_spawn) — safe after Domain.spawn.

   Two separate leak classes this guards against:
   - FD leak on exceptions: if read_all raised, close_ic was skipped and
     the pipe read-end stayed open. Under heavy load (thousands of git
     subprocesses) this quickly exhausted the per-process FD limit.
   - Zombie leak on exceptions: if close_ic raised, waitpid_noeintr was
     never called and the child became a zombie — visible as hundreds
     of <defunct> entries under [urme init] in ps.

   Fix: run both cleanups in finalize. The exit-status reporting path
   shares the same waitpid so we only wait once. *)
let run_process prog argv =
  (* Allocate all FDs first, then spawn inside a try. If create_process
     raises (EMFILE under load, ENOMEM, etc.) we must close every FD we
     already allocated — otherwise they leak for good and snowball into
     more EMFILEs. *)
  let rd, wr = Unix.pipe ~cloexec:true () in
  let devnull_out = Unix.openfile "/dev/null" [Unix.O_WRONLY; Unix.O_CLOEXEC] 0 in
  let devnull_in  = Unix.openfile "/dev/null" [Unix.O_RDONLY; Unix.O_CLOEXEC] 0 in
  let safe_close fd = try Unix.close fd with _ -> () in
  let pid =
    try Unix.create_process prog argv devnull_in wr devnull_out
    with e ->
      safe_close rd; safe_close wr;
      safe_close devnull_in; safe_close devnull_out;
      raise e
  in
  safe_close wr;
  safe_close devnull_in;
  safe_close devnull_out;
  let fd = Lwt_unix.of_unix_file_descr rd in
  let ic = Lwt_io.of_fd ~mode:Lwt_io.input fd in
  let ic_closed = ref false in
  let reaped_status = ref None in
  let close_ic () =
    if !ic_closed then Lwt.return_unit
    else begin ic_closed := true;
      Lwt.catch (fun () -> Lwt_io.close ic) (fun _ -> Lwt.return_unit)
    end
  in
  let reap () =
    match !reaped_status with
    | Some _ -> Lwt.return_unit
    | None ->
      Lwt.catch
        (fun () ->
          let* _pid, status = waitpid_noeintr pid in
          reaped_status := Some status; Lwt.return_unit)
        (fun _ ->
          reaped_status := Some (Unix.WEXITED 255); Lwt.return_unit)
  in
  Lwt.finalize
    (fun () ->
      let buf = Buffer.create 256 in
      let rec read_all () =
        let* line = Lwt_io.read_line_opt ic in
        match line with
        | Some l ->
          if Buffer.length buf > 0 then Buffer.add_char buf '\n';
          Buffer.add_string buf l;
          read_all ()
        | None -> Lwt.return_unit
      in
      let* () = read_all () in
      let* () = close_ic () in
      let* () = reap () in
      let status = match !reaped_status with
        | Some s -> s | None -> Unix.WEXITED 255 in
      Lwt.return (Buffer.contents buf, status))
    (fun () ->
      let* () = close_ic () in
      reap ())

(* Run a git command, return stdout (stderr suppressed) *)
let run_git ~cwd args =
  let argv = Array.of_list ("git" :: "-C" :: cwd :: args) in
  let* (output, status) = run_process "git" argv in
  match status with
  | Unix.WEXITED 0 -> Lwt.return output
  | Unix.WEXITED n ->
    Lwt.fail_with (Printf.sprintf "git %s failed (exit %d)"
      (String.concat " " args) n)
  | _ ->
    Lwt.fail_with (Printf.sprintf "git %s killed by signal"
      (String.concat " " args))

(* Run a shell command, return stdout *)
let run_shell cmd =
  let argv = [| "/bin/sh"; "-c"; cmd |] in
  let* (output, status) = run_process "/bin/sh" argv in
  match status with
  | Unix.WEXITED 0 -> Lwt.return output
  | Unix.WEXITED n ->
    Lwt.fail_with (Printf.sprintf "shell command failed (exit %d): %s" n cmd)
  | _ ->
    Lwt.fail_with (Printf.sprintf "shell command killed: %s" cmd)

let run_git_opt ~cwd args =
  Lwt.catch
    (fun () -> let+ out = run_git ~cwd args in Some out)
    (fun _exn -> Lwt.return None)

(* Restore code to a snapshot/commit state *)
let restore_snapshot ~cwd ~sha =
  let* _out = run_git ~cwd ["checkout"; "--force"; sha; "--"; "."] in
  Lwt.return_unit

(* Create a real commit: stage files + commit with message.
   Returns the new commit SHA. *)
let create_commit ~cwd ~message ?(paths=[]) () =
  (* Stage files *)
  let* () = match paths with
    | [] ->
      (* Stage all changes *)
      let* _out = run_git ~cwd ["add"; "-A"] in
      Lwt.return_unit
    | files ->
      let* _out = run_git ~cwd (["add"; "--"] @ files) in
      Lwt.return_unit
  in
  (* Check if there's anything to commit *)
  let* status = run_git_opt ~cwd ["diff"; "--cached"; "--quiet"] in
  match status with
  | Some _ ->
    (* Nothing staged — try staging everything *)
    let* _ = run_git_opt ~cwd ["add"; "-A"] in
    let* status2 = run_git_opt ~cwd ["diff"; "--cached"; "--quiet"] in
    (match status2 with
     | Some _ ->
       (* Still nothing — return current HEAD *)
       let+ out = run_git ~cwd ["rev-parse"; "HEAD"] in
       String.trim out
     | None ->
       let* _out = run_git ~cwd ["commit"; "-m"; message] in
       let+ out = run_git ~cwd ["rev-parse"; "HEAD"] in
       String.trim out)
  | None ->
    let* _out = run_git ~cwd ["commit"; "-m"; message] in
    let+ out = run_git ~cwd ["rev-parse"; "HEAD"] in
    String.trim out

(* Get current HEAD SHA — kept for simple cases where Irmin overhead isn't needed *)
let current_head ~cwd =
  let+ out = run_git ~cwd ["rev-parse"; "HEAD"] in
  String.trim out

(* Get current branch name *)
let current_branch ~cwd =
  let+ out = run_git ~cwd ["rev-parse"; "--abbrev-ref"; "HEAD"] in
  String.trim out

(* Show diff since a given SHA — used as fallback when Irmin diff isn't available *)
let diff_since ~cwd ~sha =
  Lwt.catch
    (fun () -> run_git ~cwd ["diff"; sha; "HEAD"])
    (fun _exn -> Lwt.return "")

(* Run git blame --porcelain and parse output.
   Returns (sha, line_number, line_content) list. *)
let blame ~cwd ?line_range ~filepath () =
  let args = ["blame"; "--porcelain"] @
    (match line_range with
     | Some (start, end_) -> [Printf.sprintf "-L%d,%d" start end_]
     | None -> []) @
    ["--"; filepath] in
  let* output = run_git ~cwd args in
  let lines = String.split_on_char '\n' output in
  (* Parse porcelain: each block starts with "<40-hex-sha> orig final [count]",
     followed by header lines, then a content line prefixed with \t *)
  let rec parse acc sha final_line = function
    | [] -> List.rev acc
    | line :: rest when String.length line > 0 && line.[0] = '\t' ->
      let content = String.sub line 1 (String.length line - 1) in
      parse ((sha, final_line, content) :: acc) sha final_line rest
    | line :: rest ->
      let parts = String.split_on_char ' ' line in
      (match parts with
       | s :: _orig :: final :: _ when String.length s >= 40 ->
         let ln = try int_of_string final with _ -> final_line in
         parse acc s ln rest
       | _ ->
         parse acc sha final_line rest)
  in
  Lwt.return (parse [] "" 0 lines)

let changed_files_since ~cwd ~sha =
  Lwt.catch
    (fun () ->
      let+ out = run_git ~cwd ["diff"; "--name-only"; sha; "HEAD"] in
      String.split_on_char '\n' out
      |> List.filter (fun s -> String.length s > 0))
    (fun _exn -> Lwt.return [])

(* Get the diff for a specific commit *)
let commit_diff ~cwd ~sha =
  Lwt.catch
    (fun () -> run_git ~cwd ["diff"; sha ^ "^"; sha])
    (fun _exn ->
      (* Initial commit — use diff-tree --root for the root commit *)
      Lwt.catch
        (fun () -> run_git ~cwd ["diff-tree"; "--root"; "-p"; sha])
        (fun _exn -> Lwt.return ""))

(* Get changed files for a specific commit *)
let commit_changed_files ~cwd ~sha =
  Lwt.catch
    (fun () ->
      let+ out = run_git ~cwd ["diff"; "--name-only"; sha ^ "^"; sha] in
      String.split_on_char '\n' out
      |> List.filter (fun s -> String.length s > 0))
    (fun _exn ->
      (* Initial commit *)
      Lwt.catch
        (fun () ->
          let+ out = run_git ~cwd ["diff-tree"; "--no-commit-id"; "--name-only"; "-r"; sha] in
          String.split_on_char '\n' out
          |> List.filter (fun s -> String.length s > 0))
        (fun _exn -> Lwt.return []))

(* Walk git log — returns list of (sha, timestamp, message) *)
let walk_log ~cwd ?(since="") ?(max_count=1000) ?(all=false) ?(branch="") () =
  let args = ["log"; "--format=%H%n%at%n%s%n---"] @
    (if all then ["--all"] else []) @
    (if since <> "" then ["--since=" ^ since] else []) @
    ["--max-count=" ^ string_of_int max_count] @
    (if branch <> "" then [branch] else []) in
  let* output = Lwt.catch
    (fun () -> run_git ~cwd args)
    (fun _exn -> Lwt.return "") in
  if output = "" then Lwt.return []
  else begin
    let lines = String.split_on_char '\n' output in
    let rec parse_entries = function
      | sha :: ts :: msg :: "---" :: rest ->
        let timestamp = try float_of_string ts with _ -> 0.0 in
        (sha, timestamp, msg) :: parse_entries rest
      | _ -> []
    in
    Lwt.return (parse_entries lines)
  end

open Lwt.Syntax

(* Run a git command, return stdout (stderr suppressed) *)
let run_git ~cwd args =
  let devnull = Unix.openfile "/dev/null" [Unix.O_WRONLY] 0 in
  let proc = Lwt_process.open_process_in ~stderr:(`FD_move devnull)
    ("git", Array.of_list ("git" :: "-C" :: cwd :: args)) in
  let buf = Buffer.create 256 in
  let rec read_all () =
    let* line = Lwt_io.read_line_opt proc#stdout in
    match line with
    | Some l ->
      if Buffer.length buf > 0 then Buffer.add_char buf '\n';
      Buffer.add_string buf l;
      read_all ()
    | None -> Lwt.return_unit
  in
  let* () = read_all () in
  let* status = proc#close in
  match status with
  | Unix.WEXITED 0 -> Lwt.return (Buffer.contents buf)
  | Unix.WEXITED n ->
    Lwt.fail_with (Printf.sprintf "git %s failed (exit %d)"
      (String.concat " " args) n)
  | _ ->
    Lwt.fail_with (Printf.sprintf "git %s killed by signal"
      (String.concat " " args))

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
let walk_log ~cwd ?(since="") ?(max_count=1000) () =
  let args = ["log"; "--format=%H%n%at%n%s%n---"] @
    (if since <> "" then ["--since=" ^ since] else []) @
    ["--max-count=" ^ string_of_int max_count] in
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

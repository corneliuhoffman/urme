open Lwt.Syntax

let run_git ~cwd args =
  let proc = Lwt_process.open_process_in
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

let get_current_sha ~cwd =
  let+ output = run_git ~cwd ["rev-parse"; "HEAD"] in
  String.trim output

let has_tracked_changes ~cwd =
  let+ output = run_git ~cwd ["status"; "--porcelain"; "-uno"] in
  String.length (String.trim output) > 0

let micro_commit ~cwd ~label ~step_num =
  let* changed = has_tracked_changes ~cwd in
  if changed then begin
    let msg = Printf.sprintf "[experience-agent] step %d: %s" step_num label in
    let* _ = run_git ~cwd ["add"; "-u"] in
    let* _ = run_git ~cwd ["commit"; "-m"; msg] in
    get_current_sha ~cwd
  end else
    get_current_sha ~cwd

let rollback_to ~cwd ~sha =
  let+ _ = run_git ~cwd ["reset"; "--hard"; sha] in
  ()

let squash_commit ~cwd ~message ~base_sha =
  let* _ = run_git ~cwd ["reset"; "--soft"; base_sha] in
  let* _ = run_git ~cwd ["commit"; "-m"; message; "--allow-empty"] in
  get_current_sha ~cwd

let diff_between ~cwd ~from_sha ~to_sha =
  run_git ~cwd ["diff"; from_sha; to_sha]

let changed_files ~cwd ~from_sha =
  let+ output = run_git ~cwd ["diff"; "--name-only"; from_sha; "HEAD"] in
  String.split_on_char '\n' output
  |> List.filter (fun s -> String.length s > 0)

(* Export / import for the urme V2 store.

   [export] uses SQLite's [VACUUM INTO <path>] to write a single
   self-contained copy of the live database. Works regardless of WAL
   state, doesn't require the source to be closed, and produces a
   standard SQLite file anyone can open.

   [import] replaces the current project's DB with a provided backup.
   Refuses to clobber an existing DB unless [~force:true].

   Neither operation tries to merge — the plan §7 marks multi-machine
   merge as deferred. "Restore" here means "fresh copy". *)

module S = Sqlite3
module D = Db

let export ~db ~path =
  (* VACUUM INTO refuses to overwrite. Clear the target first. *)
  (try Sys.remove path with _ -> ());
  let quoted = String.concat "''" (String.split_on_char '\'' path) in
  D.exec db (Printf.sprintf "VACUUM INTO '%s'" quoted)

let export_project ~project_dir ~path =
  let db = Schema.open_or_create ~project_dir in
  export ~db ~path;
  Schema.close db

(* ---------- Scoped export (branch / commit list) ----------

   Build a fresh [.urmedb] containing only the rows touching the given
   set of commit SHAs. Intended for PR reviews: the author runs
   [urme export --branch feat/foo] and hands the resulting file to a
   reviewer who imports it, so URME (and Claude via MCP) only sees
   the session context for that branch's changes.

   Inclusion rule:
   - [edit_links] where [commit_sha] ∈ commits.
   - [steps] joined to those edit_links by [(session_id, turn_index)] —
     the turns that actually produced the branch's edits.
   - [sessions] for those steps.
   - [meta] is copied whole (schema version etc.).

   FTS5's [steps_fts] rebuilds automatically from its triggers as we
   insert rows; no manual backfill needed. *)

let sql_escape_list xs =
  String.concat ", "
    (List.map (fun s ->
      "'" ^ String.concat "''" (String.split_on_char '\'' s) ^ "'") xs)

let export_scoped ~project_dir ~commits ~out_path =
  if commits = [] then
    failwith "export: no commits in scope";
  (* Fresh empty target, initialised with current schema. *)
  (try Sys.remove out_path with _ -> ());
  List.iter (fun suffix ->
    let p = out_path ^ suffix in
    if Sys.file_exists p then (try Sys.remove p with _ -> ())
  ) ["-wal"; "-shm"];
  let dst = S.db_open out_path in
  Schema.init dst;
  let src_path = Schema.default_path ~project_dir in
  let attach_sql =
    Printf.sprintf "ATTACH DATABASE '%s' AS src"
      (String.concat "''" (String.split_on_char '\'' src_path)) in
  D.exec dst attach_sql;
  let commits_csv = sql_escape_list commits in
  (* Insertion order matters — [steps.session_id] → [sessions.id] is
     a foreign key, so sessions must land first. Compute scoped steps
     / sessions via subqueries against [src] rather than the (still
     empty) local tables. *)
  let scoped_steps_predicate =
    Printf.sprintf
      "s.id IN ( \
         SELECT DISTINCT st.id FROM src.steps st \
         JOIN src.edit_links el \
           ON el.session_id = st.session_id \
          AND el.turn_idx   = st.turn_index \
         WHERE el.commit_sha IN (%s) \
       ) OR s.commit_after IN (%s)"
      commits_csv commits_csv in
  (* 1. sessions whose id appears in any scoped step *)
  D.exec dst (Printf.sprintf
    "INSERT INTO sessions \
       SELECT * FROM src.sessions \
       WHERE id IN ( \
         SELECT DISTINCT s.session_id FROM src.steps s \
         WHERE s.session_id IS NOT NULL AND (%s))"
    scoped_steps_predicate);
  (* 2. steps in scope *)
  D.exec dst (Printf.sprintf
    "INSERT INTO steps SELECT s.* FROM src.steps s WHERE %s"
    scoped_steps_predicate);
  (* 3. edit_links landing on scope commits *)
  D.exec dst (Printf.sprintf
    "INSERT INTO edit_links \
       SELECT * FROM src.edit_links WHERE commit_sha IN (%s)"
    commits_csv);
  (* keep meta (schema_version etc.) from source where not already set *)
  D.exec dst
    "INSERT OR IGNORE INTO meta SELECT * FROM src.meta";
  D.exec dst "DETACH DATABASE src";
  ignore (S.db_close dst)

(* Copy a file byte-for-byte. Not using Unix.link — we want a clean copy,
   not a hardlink that would couple source and destination. *)
let copy_file ~src ~dst =
  let ic = open_in_bin src in
  let oc = open_out_bin dst in
  let buf = Bytes.create 65536 in
  let rec loop () =
    let n = input ic buf 0 (Bytes.length buf) in
    if n > 0 then (output oc buf 0 n; loop ())
  in
  (try loop () with _ -> ());
  close_in ic;
  close_out oc

let verify_valid_db path =
  (* Open the source and run the schema init (idempotent) so we reject
     garbage files early, and upgrade older dumps to the current schema. *)
  let db = S.db_open path in
  Schema.init db;
  ignore (S.db_close db)

let import_project ?(force=false) ~project_dir ~path () =
  if not (Sys.file_exists path) then
    failwith (Printf.sprintf "import: source %s not found" path);
  let target = Schema.default_path ~project_dir in
  if Sys.file_exists target && not force then
    failwith (Printf.sprintf "import: %s already exists (use --force)" target);
  Schema.ensure_parent_dir target;
  (* Sanity check the source before we clobber anything. *)
  verify_valid_db path;
  (* Remove WAL/SHM siblings for a clean slate. *)
  List.iter (fun suffix ->
    let p = target ^ suffix in
    if Sys.file_exists p then (try Sys.remove p with _ -> ())
  ) ["-wal"; "-shm"; ""];
  copy_file ~src:path ~dst:target

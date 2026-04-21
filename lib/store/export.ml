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

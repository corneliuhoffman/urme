(* Access layer for the [edit_links] table.

   One row per edit_key. Takes over what ChromaDB's `git_info` metadata blob
   used to store — per-edit (commit_sha, diff_hash) bookkeeping.

   commit_sha = NULL means Pending or Dead (the walker hasn't matched this
   edit to a commit yet, or has determined it never will). The old code
   distinguished Pending/Dead via entry presence/absence; here we use a
   single nullable column since the distinction didn't affect reads. *)

module S = Sqlite3
module D = Db

type row = {
  edit_key    : string;
  session_id  : string option;
  turn_idx    : int;
  entry_idx   : int;
  file_base   : string;
  commit_sha  : string option;
  diff_hash   : string;
  origin      : string;    (* "claude" | "human" *)
  branch      : string;
  timestamp   : float;
  old_content : string;    (* "" for Write/human-reconcile-on-absent *)
  new_content : string;    (* content the edit wrote *)
}

let row_of_cols cols =
  { edit_key    = D.data_to_string cols.(0);
    session_id  = D.data_to_string_opt cols.(1);
    turn_idx    = D.data_to_int cols.(2);
    entry_idx   = D.data_to_int cols.(3);
    file_base   = D.data_to_string cols.(4);
    commit_sha  = D.data_to_string_opt cols.(5);
    diff_hash   = D.data_to_string cols.(6);
    origin      = D.data_to_string cols.(7);
    branch      = D.data_to_string cols.(8);
    timestamp   = D.data_to_float cols.(9);
    old_content = D.data_to_string cols.(10);
    new_content = D.data_to_string cols.(11) }

let select_cols =
  "edit_key, session_id, turn_idx, entry_idx, file_base, \
   commit_sha, diff_hash, origin, branch, timestamp, \
   COALESCE(old_content,''), COALESCE(new_content,'')"

(* Upsert a row. commit_sha = None means clear the link back to pending. *)
let upsert db row =
  let sid =
    match row.session_id with Some s -> S.Data.TEXT s | None -> S.Data.NULL in
  let csha =
    match row.commit_sha with Some s -> S.Data.TEXT s | None -> S.Data.NULL in
  D.exec_params db
    "INSERT INTO edit_links(\
       edit_key, session_id, turn_idx, entry_idx, file_base, \
       commit_sha, diff_hash, origin, branch, timestamp, \
       old_content, new_content) \
     VALUES(?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?) \
     ON CONFLICT(edit_key) DO UPDATE SET \
       session_id  = excluded.session_id, \
       turn_idx    = excluded.turn_idx, \
       entry_idx   = excluded.entry_idx, \
       file_base   = excluded.file_base, \
       commit_sha  = excluded.commit_sha, \
       diff_hash   = excluded.diff_hash, \
       origin      = excluded.origin, \
       branch      = excluded.branch, \
       timestamp   = excluded.timestamp, \
       old_content = excluded.old_content, \
       new_content = excluded.new_content"
    [ S.Data.TEXT row.edit_key;
      sid;
      S.Data.INT (Int64.of_int row.turn_idx);
      S.Data.INT (Int64.of_int row.entry_idx);
      S.Data.TEXT row.file_base;
      csha;
      S.Data.TEXT row.diff_hash;
      S.Data.TEXT row.origin;
      S.Data.TEXT row.branch;
      S.Data.FLOAT row.timestamp;
      S.Data.TEXT row.old_content;
      S.Data.TEXT row.new_content ]

(* Mark an edit pending/dead by clearing commit_sha. Keeps the rest of the
   row intact so the walker can revisit it later. *)
let mark_orphan db ~edit_key =
  D.exec_params db
    "UPDATE edit_links SET commit_sha = NULL WHERE edit_key = ?"
    [S.Data.TEXT edit_key]

let get db ~edit_key =
  let sql = Printf.sprintf "SELECT %s FROM edit_links WHERE edit_key = ?"
      select_cols in
  let rows = D.query_list db sql [S.Data.TEXT edit_key] ~f:row_of_cols in
  match rows with r :: _ -> Some r | [] -> None

let all db =
  D.query_list db
    (Printf.sprintf "SELECT %s FROM edit_links ORDER BY timestamp" select_cols)
    [] ~f:row_of_cols

let all_with_commit db =
  D.query_list db
    (Printf.sprintf "SELECT %s FROM edit_links \
                     WHERE commit_sha IS NOT NULL ORDER BY timestamp"
       select_cols)
    [] ~f:row_of_cols

(* For a specific commit_sha, return every edit that was linked to it. *)
let for_commit db ~commit_sha =
  D.query_list db
    (Printf.sprintf "SELECT %s FROM edit_links WHERE commit_sha = ? \
                     ORDER BY timestamp"
       select_cols)
    [S.Data.TEXT commit_sha] ~f:row_of_cols

(* For a specific file basename, every linkage row (committed or not). *)
let for_file_base db ~file_base =
  D.query_list db
    (Printf.sprintf "SELECT %s FROM edit_links WHERE file_base = ? \
                     ORDER BY timestamp"
       select_cols)
    [S.Data.TEXT file_base] ~f:row_of_cols

(* For a (session_id, turn_idx) — the old ChromaDB "interaction" grouping. *)
let for_turn db ~session_id ~turn_idx =
  D.query_list db
    (Printf.sprintf "SELECT %s FROM edit_links \
                     WHERE session_id = ? AND turn_idx = ? \
                     ORDER BY entry_idx"
       select_cols)
    [S.Data.TEXT session_id; S.Data.INT (Int64.of_int turn_idx)]
    ~f:row_of_cols

(* Remove a row entirely (rare — typically used when wiping). *)
let delete db ~edit_key =
  D.exec_params db "DELETE FROM edit_links WHERE edit_key = ?"
    [S.Data.TEXT edit_key]

let delete_all db =
  D.exec db "DELETE FROM edit_links"

(* Orphan cleanup: clear commit_sha on any row whose stored commit no
   longer exists in the [reachable] set (unreachable after rebase/branch
   delete, etc.). Returns count of affected rows. *)
let cleanup_orphans db ~reachable_shas =
  let rows = all_with_commit db in
  let n = ref 0 in
  List.iter (fun r ->
    match r.commit_sha with
    | Some sha when not (List.mem sha reachable_shas) ->
      mark_orphan db ~edit_key:r.edit_key;
      incr n
    | _ -> ()
  ) rows;
  !n

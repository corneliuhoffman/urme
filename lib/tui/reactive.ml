(* Nottui/Lwd reactive git view for urme.
   Built incrementally:
     step 1  text + quit        [done]
     step 2  pre-load git data  [done]
     step 3  mouse off for copy [done]
     step 4  4 reactive panels  [this]
*)

open Lwt.Syntax
module U = Nottui.Ui
module W = Nottui_widgets
module A = Notty.A
module I = Notty.I

(* Route Lwd's "unsafe mutation" warnings to the dbg log instead of
   stderr (which would corrupt the Nottui display). If it's firing
   on our event-handler mutations, it means Lwd saw the [Lwd.set]
   but classified the context as unsafe — which can prevent proper
   invalidation propagation. *)
let () =
  Lwd.unsafe_mutation_logger := (fun () ->
    let oc = open_out_gen [Open_append; Open_creat] 0o644
        "/tmp/urme_reactive.log" in
    Printf.fprintf oc "%.3f UNSAFE MUTATION detected\n"
      (Unix.gettimeofday ());
    close_out oc)

let dbg msg =
  try
    let oc = open_out_gen [Open_append; Open_creat] 0o644
        "/tmp/urme_reactive.log" in
    Printf.fprintf oc "%.3f %s\n" (Unix.gettimeofday ()) msg;
    close_out oc
  with _ -> ()

type link = Urme_engine.Git_link_types.link
type focus = Branches | Commits | Files | Links
type hist_focus = Sessions | Turns
type hist_view = Overview | Body
type mode = Git | History | Search
type search_focus = Query | Results

type turn_info = {
  t_turn_idx   : int;
  t_timestamp  : float;
  t_prompt     : string;
  t_summary    : string;
  t_files      : string list;
  t_commands   : string list;
}

type session_info = {
  session_id   : string;
  started_at   : float;
  first_prompt : string;
  turns        : turn_info list;
}

type data = {
  branches : string list;
  current_branch : string;
  commits : (string * float * string) list;
  git_links : (string * string, link list) Hashtbl.t;
  project_dir : string;
  sessions : session_info list;  (* newest first *)
}

(* -------- Pre-load git data (BEFORE Nottui takes the tty) -------- *)

let load_data ~project_dir : data Lwt.t =
  let cwd = project_dir in
  let* out = Urme_git.Ops.run_git ~cwd ["branch"; "--list"; "--no-color"] in
  let branches =
    String.split_on_char '\n' out
    |> List.filter_map (fun s ->
      let s = String.trim s in
      if s = "" then None
      else Some (if String.length s > 2 && s.[0] = '*'
                 then String.trim (String.sub s 2 (String.length s - 2))
                 else s)) in
  let* current_branch =
    Lwt.catch (fun () -> Urme_git.Ops.current_branch ~cwd)
      (fun _ -> Lwt.return "") in
  let* commits =
    Lwt.catch (fun () ->
      Urme_git.Ops.walk_log ~cwd ~branch:current_branch ~max_count:200 ())
      (fun _ -> Lwt.return []) in
  let git_links =
    try
      let db = Urme_store.Schema.open_or_create ~project_dir in
      let all = Urme_store.Edit_links.all_with_commit db in
      let has_claude = Hashtbl.create 128 in
      List.iter (fun (row : Urme_store.Edit_links.row) ->
        if row.origin = "claude" then
          match row.commit_sha with
          | Some sha -> Hashtbl.replace has_claude (sha, row.file_base) ()
          | None -> ()
      ) all;
      let links = Hashtbl.create 256 in
      List.iter (fun (row : Urme_store.Edit_links.row) ->
        match row.commit_sha with
        | None -> ()
        | Some commit_sha ->
          let shadowed =
            row.origin = "human"
            && Hashtbl.mem has_claude (commit_sha, row.file_base) in
          if not shadowed then begin
            let session_id = Option.value row.session_id ~default:"human" in
            let entry : link = {
              commit_sha; file = row.file_base; session_id;
              turn_idx = row.turn_idx; entry_idx = row.entry_idx;
              edit_key = row.edit_key } in
            let existing =
              match Hashtbl.find_opt links (commit_sha, row.file_base)
              with Some l -> l | None -> [] in
            if not (List.exists
                      (fun (l : link) -> l.edit_key = row.edit_key) existing)
            then Hashtbl.replace links (commit_sha, row.file_base)
                   (existing @ [entry])
          end
      ) all;
      Urme_store.Schema.close db;
      links
    with _ -> Hashtbl.create 0 in
  let sessions =
    try
      let db = Urme_store.Schema.open_or_create ~project_dir:cwd in
      (* Per-session metadata: earliest timestamp + first prompt. *)
      let session_rows = Urme_store.Db.query_list db
        "SELECT s.session_id, MIN(s.timestamp) AS t, \
         (SELECT prompt_text FROM steps \
          WHERE session_id = s.session_id \
          ORDER BY turn_index LIMIT 1) \
         FROM steps s \
         WHERE s.session_id IS NOT NULL \
         GROUP BY s.session_id \
         ORDER BY t DESC"
        [] ~f:(fun cols ->
          (Urme_store.Db.data_to_string cols.(0),
           Urme_store.Db.data_to_float cols.(1),
           Urme_store.Db.data_to_string cols.(2))) in
      let parse_arr s =
        try match Yojson.Safe.from_string s with
          | `List xs ->
            List.filter_map (function `String s -> Some s | _ -> None) xs
          | _ -> []
        with _ -> [] in
      let sessions = List.map (fun (sid, started_at, first) ->
        let first_prompt =
          let t = String.trim first in
          if String.length t > 80 then String.sub t 0 80 ^ "…" else t in
        let turns = Urme_store.Db.query_list db
          "SELECT turn_index, timestamp, \
                  COALESCE(prompt_text,''), COALESCE(summary,''), \
                  COALESCE(files_touched,'[]'), COALESCE(commands_run,'[]') \
           FROM steps WHERE session_id = ? ORDER BY turn_index"
          [Sqlite3.Data.TEXT sid]
          ~f:(fun cols ->
            { t_turn_idx  = Urme_store.Db.data_to_int cols.(0);
              t_timestamp = Urme_store.Db.data_to_float cols.(1);
              t_prompt    = Urme_store.Db.data_to_string cols.(2);
              t_summary   = Urme_store.Db.data_to_string cols.(3);
              t_files     = parse_arr (Urme_store.Db.data_to_string cols.(4));
              t_commands  = parse_arr (Urme_store.Db.data_to_string cols.(5)); })
        in
        { session_id = sid; started_at; first_prompt; turns }
      ) session_rows in
      Urme_store.Schema.close db;
      sessions
    with _ -> []
  in
  Lwt.return { branches; current_branch; commits; git_links;
               project_dir = cwd; sessions }

(* -------- State -------- *)

type state = {
  data         : data;
  mode         : mode Lwd.var;
  (* git mode *)
  focus        : focus Lwd.var;
  branch_idx   : int Lwd.var;
  commit_idx   : int Lwd.var;
  file_idx     : int Lwd.var;
  link_idx     : int Lwd.var;
  diff_scroll  : int Lwd.var;
  commits      : (string * float * string) list Lwd.var;
  current_diff : string Lwd.var;
  (* history mode *)
  hist_focus   : hist_focus Lwd.var;
  hist_view    : hist_view Lwd.var;
  (* git/links: offset from the selected link's turn so you can
     step back/forward to see surrounding turns for context. *)
  link_turn_offset : int Lwd.var;
  session_idx  : int Lwd.var;
  turn_idx     : int Lwd.var;
  hist_scroll  : int Lwd.var;
  (* search mode *)
  search_focus   : search_focus Lwd.var;
  search_query   : (string * int) Lwd.var;
  search_hits    : Urme_search.Search.hit list Lwd.var;
  search_idx     : int Lwd.var;
  search_scroll  : int Lwd.var;
  search_running : bool Lwd.var;
  search_view    : hist_view Lwd.var;
  search_turn_offset : int Lwd.var;
  search_synthesis   : string Lwd.var;
  (* Filled in by [root] once config is loaded. *)
  mutable run_search : (string -> unit);
  quit_u       : unit Lwt.u;
}

let run_git_detached ~cwd ~args : string Lwt.t =
  Lwt_preemptive.detach (fun () ->
    let rd, wr = Unix.pipe ~cloexec:true () in
    let dev_in = Unix.openfile "/dev/null" [Unix.O_RDONLY] 0 in
    let dev_out = Unix.openfile "/dev/null" [Unix.O_WRONLY] 0 in
    let argv = Array.of_list ("git" :: "-C" :: cwd :: args) in
    let pid = Unix.create_process "git" argv dev_in wr dev_out in
    Unix.close wr; Unix.close dev_in; Unix.close dev_out;
    let ic = Unix.in_channel_of_descr rd in
    let buf = Buffer.create 4096 in
    let chunk = Bytes.create 4096 in
    (try
       while true do
         let n = input ic chunk 0 (Bytes.length chunk) in
         if n = 0 then raise End_of_file
         else Buffer.add_subbytes buf chunk 0 n
       done
     with End_of_file -> ());
    close_in ic;
    let _ = Unix.waitpid [] pid in
    Buffer.contents buf) ()

(* Find the JSONL file for a session and extract the turn that
   produced this edit. Returns (user_text, assistant_text) if found. *)
let fetch_turn_for ~project_dir (link : link) =
  if link.session_id = "human" || link.session_id = "" then None
  else
    let jsonl_dir = Urme_search.Jsonl_reader.find_jsonl_dir ~project_dir in
    let filepath = Filename.concat jsonl_dir (link.session_id ^ ".jsonl") in
    if not (Sys.file_exists filepath) then None
    else
      let interactions =
        Urme_search.Jsonl_reader.parse_interactions ~filepath in
      List.find_opt (fun (i : Urme_core.Types.interaction) ->
        i.index = link.turn_idx) interactions

(* For a selected link, render: user prompt, assistant response, and
   the edit's own old→new content. For "human" edits (no session)
   just show the raw content diff. *)
let link_content_diff s (link : link) =
  let edit_block =
    try
      let db = Urme_store.Schema.open_or_create
          ~project_dir:s.data.project_dir in
      let row = Urme_store.Edit_links.get db ~edit_key:link.edit_key in
      Urme_store.Schema.close db;
      match row with
      | Some r ->
        let lines prefix content =
          String.split_on_char '\n' content
          |> List.map (fun line -> prefix ^ line)
          |> String.concat "\n" in
        Printf.sprintf "--- old\n+++ new\n%s\n%s"
          (lines "-" r.old_content) (lines "+" r.new_content)
      | None -> "(edit row not found)"
    with e -> Printf.sprintf "(error: %s)" (Printexc.to_string e)
  in
  match fetch_turn_for ~project_dir:s.data.project_dir link with
  | None -> edit_block
  | Some i ->
    Printf.sprintf
      "# turn %d\n\n> user:\n%s\n\n> assistant:\n%s\n\n> edit:\n%s"
      i.index i.user_text i.assistant_summary edit_block

(* Purely-derived Lwd.t of "what should appear in the right panel"
   given the current focus + all index vars. Returns either an
   immediate string (for Links, which we render inline from the DB)
   or a list of git args that need an async subprocess. *)
type right_content =
  | Immediate of string
  | Needs_fetch of string list

let wanted_content s : right_content Lwd.t =
  let focus  = Lwd.get s.focus in
  let b_idx  = Lwd.get s.branch_idx in
  let c_idx  = Lwd.get s.commit_idx in
  let f_idx  = Lwd.get s.file_idx in
  let l_idx  = Lwd.get s.link_idx in
  let commits = Lwd.get s.commits in
  let tuple = Lwd.pair
    (Lwd.pair focus (Lwd.pair b_idx c_idx))
    (Lwd.pair (Lwd.pair f_idx l_idx) commits) in
  Lwd.map tuple
    ~f:(fun ((focus, (bi, ci)), ((fi, li), cs)) ->
      let branch =
        match List.nth_opt s.data.branches bi with
        | Some b -> b | None -> "" in
      let sha =
        match List.nth_opt cs ci with
        | Some (sha, _, _) -> sha | None -> "" in
      let files =
        if sha = "" then []
        else
          Hashtbl.fold (fun (s', fb) _ acc ->
            if s' = sha && not (List.mem fb acc) then fb :: acc else acc
          ) s.data.git_links []
          |> List.sort String.compare in
      let file = match List.nth_opt files fi with
        | Some fb -> fb | None -> "" in
      match focus with
      | Branches when branch <> "" ->
        Needs_fetch ["log"; "--format=%h %ad %s"; "--date=short";
                     "--max-count=200"; branch]
      | Commits when sha <> "" ->
        Needs_fetch ["diff"; sha ^ "^"; sha]
      | Files when sha <> "" && file <> "" ->
        (* [file] is just the basename (that's what edit_links
           stores). Glob pathspec matches it anywhere in the tree. *)
        Needs_fetch ["diff"; sha ^ "^"; sha; "--";
                     ":(glob)**/" ^ file; ":(glob)" ^ file]
      | Links when file <> "" ->
        let ls =
          match Hashtbl.find_opt s.data.git_links (sha, file) with
          | Some l ->
            List.sort (fun (a : link) b ->
              let c = Int.compare a.turn_idx b.turn_idx in
              if c <> 0 then c else Int.compare a.entry_idx b.entry_idx) l
          | None -> [] in
        (match List.nth_opt ls li with
         | Some l -> Immediate (link_content_diff s l)
         | None -> Immediate "(no link)")
      | _ -> Immediate "(nothing to show)")

(* The driver: an Lwd.t sink that reacts to [wanted_content]
   changes. Every time the derivation invalidates, this map re-runs
   and — if the content actually differs from what was last
   requested — kicks off the right fetch / writes the right string.
   The returned unit is zipped into the UI tree so the engine
   actually observes it. *)
let content_driver s =
  let last : right_content option ref = ref None in
  Lwd.map (wanted_content s) ~f:(fun rc ->
    if Some rc <> !last then begin
      last := Some rc;
      match rc with
      | Immediate str -> Lwd.set s.current_diff str
      | Needs_fetch args ->
        Lwd.set s.current_diff "(loading…)";
        Lwt.async (fun () ->
          let* text = Lwt.catch
            (fun () -> run_git_detached ~cwd:s.data.project_dir ~args)
            (fun _ -> Lwt.return "") in
          Lwd.set s.current_diff text;
          Lwt.return_unit)
    end;
    U.empty)

(* Reactive driver: [branch_idx] → selected branch name. When it
   changes, async-fetch that branch's commits and write them to
   [s.commits]. The right-panel driver above will notice and
   refresh the diff. No imperative "refresh" call needed. *)
let commits_driver s =
  let last_branch = ref None in
  let selected_branch =
    Lwd.map (Lwd.get s.branch_idx) ~f:(fun i ->
      match List.nth_opt s.data.branches i with
      | Some b -> b | None -> "") in
  Lwd.map selected_branch ~f:(fun b ->
    if Some b <> !last_branch && b <> "" then begin
      last_branch := Some b;
      Lwt.async (fun () ->
        let* raw = Lwt.catch
          (fun () -> run_git_detached ~cwd:s.data.project_dir
              ~args:["log"; "--format=%H%n%at%n%s%n---";
                     "--max-count=200"; b])
          (fun _ -> Lwt.return "") in
        let cs =
          let lines = String.split_on_char '\n' raw in
          let rec parse = function
            | sha :: ts :: msg :: "---" :: rest ->
              (sha, (try float_of_string ts with _ -> 0.0), msg) :: parse rest
            | _ -> [] in
          parse lines in
        Lwd.set s.commits cs;
        Lwd.set s.commit_idx 0;
        Lwd.set s.file_idx 0;
        Lwd.set s.link_idx 0;
        Lwt.return_unit)
    end;
    U.empty)

(* -------- Derived Lwd values -------- *)

let current_sha s : string Lwd.t =
  Lwd.map2 (Lwd.get s.commits) (Lwd.get s.commit_idx) ~f:(fun cs i ->
    match List.nth_opt cs i with
    | Some (sha, _, _) -> sha
    | None -> "")

let current_files s : string list Lwd.t =
  Lwd.map (current_sha s) ~f:(fun sha ->
    if sha = "" then []
    else
      Hashtbl.fold (fun (s', fb) _ acc ->
        if s' = sha && not (List.mem fb acc) then fb :: acc else acc
      ) s.data.git_links []
      |> List.sort String.compare)

let current_links s : link list Lwd.t =
  Lwd.map2 (current_sha s) (Lwd.get s.file_idx) ~f:(fun sha fidx ->
    if sha = "" then []
    else
      let files =
        Hashtbl.fold (fun (s', fb) _ acc ->
          if s' = sha && not (List.mem fb acc) then fb :: acc else acc
        ) s.data.git_links []
        |> List.sort String.compare in
      match List.nth_opt files fidx with
      | Some fb ->
        (match Hashtbl.find_opt s.data.git_links (sha, fb) with
         | Some l ->
           List.sort (fun (a : link) b ->
             let c = Int.compare a.turn_idx b.turn_idx in
             if c <> 0 then c else Int.compare a.entry_idx b.entry_idx) l
         | None -> [])
      | None -> [])

(* -------- Rendering -------- *)

let title_attr ~focused =
  if focused then A.(fg black ++ bg lightcyan ++ st bold)
  else A.(fg lightcyan ++ st bold)

let row_attr ~selected =
  if selected then A.(fg black ++ bg lightyellow ++ st bold)
  else A.(fg white)

(* Use notty's Unicode char filler — [I.string] with an arbitrary
   number of ─ bytes can undercount display width in edge cases. *)
let dash_img attr n =
  if n <= 0 then I.empty
  else I.uchar attr (Uchar.of_int 0x2500) n 1   (* ─ *)

(* Count display cells in a UTF-8 string. Most BMP chars = 1 cell;
   we ignore east-asian wide-char edge cases here. String.length
   counts bytes, which over-pads multi-byte lines and breaks the
   panel's right border into floating fragments. *)
let display_width s =
  let n = String.length s in
  let cells = ref 0 and i = ref 0 in
  while !i < n do
    let c = Char.code s.[!i] in
    incr cells;
    i := !i +
      (if c < 0x80 then 1
       else if c < 0xc0 then 1           (* continuation — treat as 1 *)
       else if c < 0xe0 then 2
       else if c < 0xf0 then 3
       else 4)
  done;
  !cells

(* Pad / truncate a string to exactly [cells] display cells. Truncates
   conservatively (one cell early so there's room for the "…" marker
   when we want one). *)
let pad_cells s cells =
  let w = display_width s in
  if w = cells then s
  else if w < cells then s ^ String.make (cells - w) ' '
  else
    (* Too wide — drop bytes from the end until w <= cells - 1.
       Conservative: we assume each dropped byte is part of a 1-cell
       rune. For plain ASCII this is exact. *)
    let bytes = String.length s in
    let rec shrink k =
      if k <= 0 then ""
      else
        let t = String.sub s 0 k in
        if display_width t <= cells - 1 then t ^ "…" else shrink (k - 1)
    in
    shrink bytes

(* Colour palette, hoisted above the panel renderers that reference it. *)
let c_user      = A.lightyellow
let c_assistant = A.(rgb_888 ~r:245 ~g:245 ~b:245)
let c_thinking  = A.(gray 20)
let c_tool_name = A.lightmagenta
let c_tool_res  = A.(gray 22)
let c_diff_add  = A.(rgb_888 ~r:160 ~g:255 ~b:160)
let c_diff_del  = A.(rgb_888 ~r:255 ~g:160 ~b:160)
let c_hunk      = A.lightcyan
let c_fg_main   = A.(rgb_888 ~r:245 ~g:245 ~b:245)
let c_bg        = A.black

(* Box-bordered panel. No more size_sensor — mutating Lwd.vars from
   inside a sensor callback during sampling deadlocks the Lwd graph
   (the mutation is "Unsafe", [on_invalidate] gets skipped, the root
   is permanently stuck at [Eval_none]). Use [Nottui_widgets.scrollbox]
   on the caller side for scrolling. *)
let render_panel ~idx ~title ~focused ~items ~sel_idx ~w ~h =
  if w < 4 || h < 3 then U.empty
  else
    let border_attr =
      if focused then A.(fg lightcyan ++ st bold)
      else A.(fg (gray 12)) in
    let t_attr = A.(fg lightcyan ++ st bold) in
    let inner_w = max 1 (w - 2) in
    let body_h  = max 0 (h - 2) in
    let label = Printf.sprintf "[%d]─%s" idx title in
    (* Count UTF-8 code points, not bytes — "─" is 1 display cell
       but 3 bytes, so String.length over-counts and the top border
       ends up short by 2 cells per ─ in the label. *)
    let lbl_cells =
      let b = ref 0 and i = ref 0 and n = String.length label in
      while !i < n do
        let c = Char.code label.[!i] in
        incr b;
        i := !i +
          (if c < 0x80 then 1
           else if c < 0xc0 then 1
           else if c < 0xe0 then 2
           else if c < 0xf0 then 3
           else 4)
      done;
      !b in
    let fill_n = max 0 (inner_w - lbl_cells - 1) in
    let top =
      I.hcat [
        I.string border_attr "┌─";
        I.string t_attr label;
        dash_img border_attr fill_n;
        I.string border_attr "┐";
      ] in
    let bot =
      I.hcat [
        I.string border_attr "└";
        dash_img border_attr inner_w;
        I.string border_attr "┘";
      ] in
    let n = List.length items in
    let scroll = max 0 (min (max 0 (n - body_h)) (sel_idx - body_h / 2)) in
    let make_row i label =
      let is_sel = i = sel_idx in
      let attr =
        if is_sel && focused then
          A.(fg black ++ bg lightyellow ++ st bold)
        else if is_sel then
          (* Unfocused selection: dim inverse so it's still visible
             which row was last selected in each panel. *)
          A.(fg lightyellow ++ bg (gray 4) ++ st bold)
        else if focused then A.(fg lightwhite)
        else A.(fg (gray 16)) in
      let content = pad_cells ("  " ^ label) inner_w in
      I.hcat [
        I.string border_attr "│";
        I.string attr content;
        I.string border_attr "│";
      ] in
    let visible =
      items
      |> List.mapi (fun i x -> (i, x))
      |> List.filter (fun (i, _) -> i >= scroll && i < scroll + body_h)
      |> List.map (fun (i, label) -> make_row i label) in
    let filler =
      I.hcat [
        I.string border_attr "│";
        I.char A.(bg black) ' ' inner_w 1;
        I.string border_attr "│";
      ] in
    let pads =
      List.init (max 0 (body_h - List.length visible)) (fun _ -> filler) in
    U.atom (I.vcat (top :: visible @ pads @ [bot]))

(* Wrap a panel so its allocated (w, h) flows back through a stable
   [size_sensor] into two Lwd.vars the panel reads. The sensor's
   callback is created once (outside any Lwd.map) so re-renders
   don't produce a new sensor each frame — previously that caused
   Nottui to call the sensor on every render, which called Lwd.set,
   which invalidated the tree, which re-rendered … = runaway loop. *)
(* Wrap a panel Lwd.t with stretch (and bonus stretch when focused).
   Content gets a fixed virtual (w, h) — scrollbox handles overflow. *)
(* Real sizing via [size_sensor]. The callback fires during the
   Lwd sample — mutating a var right then would be "Unsafe" and
   skip [on_invalidate], which broke reactivity before. Wrap each
   [Lwd.set] in [Lwt.async] so it runs between frames. *)
let with_size_sensor ~focused make_ui =
  let w_var = Lwd.var 80 in
  let h_var = Lwd.var 15 in
  let sensor : U.size_sensor = fun ~w ~h ->
    let ow = Lwd.peek w_var and oh = Lwd.peek h_var in
    if ow <> w || oh <> h then
      Lwt.async (fun () ->
        if Lwd.peek w_var <> w then Lwd.set w_var w;
        if Lwd.peek h_var <> h then Lwd.set h_var h;
        Lwt.return_unit)
  in
  let sized_ui =
    let size = Lwd.pair (Lwd.get w_var) (Lwd.get h_var) in
    Lwd.map2 size make_ui ~f:(fun (w, h) ui_of_size ->
      ui_of_size ~w ~h) in
  Lwd.map2 sized_ui focused ~f:(fun ui focused ->
    let pad = Nottui.Gravity.(make ~h:`Negative ~v:`Negative) in
    U.size_sensor sensor
      (U.resize ~w:0 ~h:0 ~sw:1 ~sh:(if focused then 3 else 1) ~pad ui))

(* -------- Panels as Lwd.t UI -------- *)

(* Wrap a UI so that left-click sets focus (also chases [chase], e.g.
   resets scroll) and scroll wheel calls [on_scroll] with +1/-1. *)
let with_mouse ~on_click ?(on_scroll=fun _ -> ()) ui_lwd =
  Lwd.map ui_lwd ~f:(fun ui ->
    U.mouse_area (fun ~x:_ ~y:_ btn ->
      match btn with
      | `Left | `Middle | `Right -> on_click (); `Handled
      | `Scroll `Up   -> on_scroll (-1); `Handled
      | `Scroll `Down -> on_scroll 1;    `Handled) ui)

(* Wrap a list-style panel (rendered by [render_panel]) with row-click
   support: a left-click on a body row selects that row.  Mouse_area
   callbacks receive coordinates relative to their own origin (nottui
   subtracts the parent offset), so [y=0] is the panel's top border
   and [y=1] is the first body row.  A [frame_sensor] captures the
   current body height so we know when the click falls on a row that
   actually has content.  Scroll wheel still calls [on_scroll]. *)
let with_clickable_rows ~focus_set ~items_count ~sel_var
    ?(on_scroll=fun _ -> ()) ui_lwd =
  let body_h = Lwd.var 0 in
  let sensor : U.frame_sensor = fun ~x:_ ~y:_ ~w:_ ~h () ->
    let nb = max 0 (h - 2) in
    if Lwd.peek body_h <> nb then
      Lwt.async (fun () ->
        if Lwd.peek body_h <> nb then Lwd.set body_h nb;
        Lwt.return_unit) in
  Lwd.map ui_lwd ~f:(fun ui ->
    let ui = U.permanent_sensor sensor ui in
    U.mouse_area (fun ~x:_ ~y btn ->
      match btn with
      | `Left | `Middle | `Right ->
        focus_set ();
        let bh = Lwd.peek body_h in
        let n = items_count () in
        let sel = Lwd.peek sel_var in
        let scroll = max 0 (min (max 0 (n - bh)) (sel - bh / 2)) in
        let rel = y - 1 in
        if bh > 0 && rel >= 0 && rel < bh then begin
          let target = scroll + rel in
          if target < n then Lwd.set sel_var target
        end;
        `Handled
      | `Scroll `Up   -> on_scroll (-1); `Handled
      | `Scroll `Down -> on_scroll 1;    `Handled) ui)

let branches_panel s =
  let focused = Lwd.map (Lwd.get s.focus) ~f:(fun f -> f = Branches) in
  let make =
    Lwd.map2 focused (Lwd.get s.branch_idx) ~f:(fun focused sel ~w ~h ->
      let items = List.map (fun b ->
        if b = s.data.current_branch then "* " ^ b else "  " ^ b
      ) s.data.branches in
      render_panel ~idx:1 ~title:"Branches" ~focused ~items ~sel_idx:sel ~w ~h)
  in
  let ui = with_size_sensor ~focused make in
  with_clickable_rows ui
    ~focus_set:(fun () -> Lwd.set s.focus Branches)
    ~items_count:(fun () -> List.length s.data.branches)
    ~sel_var:s.branch_idx
    ~on_scroll:(fun d ->
      let n = List.length s.data.branches in
      let clamp i = max 0 (min (max 0 (n - 1)) i) in
      Lwd.set s.focus Branches;
      Lwd.update (fun i -> clamp (i + d)) s.branch_idx)

let commits_panel s =
  let focused = Lwd.map (Lwd.get s.focus) ~f:(fun f -> f = Commits) in
  let items_lwd = Lwd.map (Lwd.get s.commits) ~f:(fun cs ->
    List.map (fun (sha, _, msg) ->
      let short = if String.length sha >= 7 then String.sub sha 0 7 else sha in
      Printf.sprintf "%s %s" short msg) cs) in
  let make =
    Lwd.map2
      (Lwd.map2 items_lwd (Lwd.get s.commit_idx)
         ~f:(fun items sel -> (items, sel)))
      focused
      ~f:(fun (items, sel) focused ~w ~h ->
        render_panel ~idx:2 ~title:"Commits"
          ~focused ~items ~sel_idx:sel ~w ~h)
  in
  let ui = with_size_sensor ~focused make in
  with_clickable_rows ui
    ~focus_set:(fun () -> Lwd.set s.focus Commits)
    ~items_count:(fun () -> List.length (Lwd.peek s.commits))
    ~sel_var:s.commit_idx
    ~on_scroll:(fun d ->
      let n = List.length (Lwd.peek s.commits) in
      let clamp i = max 0 (min (max 0 (n - 1)) i) in
      Lwd.set s.focus Commits;
      Lwd.update (fun i -> clamp (i + d)) s.commit_idx;
      Lwd.set s.file_idx 0;
      Lwd.set s.link_idx 0;
      Lwd.set s.diff_scroll 0)

let files_panel s =
  let focused = Lwd.map (Lwd.get s.focus) ~f:(fun f -> f = Files) in
  let make =
    Lwd.map2
      (Lwd.map2 (current_files s) (Lwd.get s.file_idx)
         ~f:(fun files sel -> (files, sel)))
      focused
      ~f:(fun (files, sel) focused ~w ~h ->
        render_panel ~idx:3 ~title:"Files"
          ~focused ~items:(List.map (fun fb -> "  " ^ fb) files)
          ~sel_idx:sel ~w ~h)
  in
  let ui = with_size_sensor ~focused make in
  let current_sha_peek () =
    match List.nth_opt (Lwd.peek s.commits) (Lwd.peek s.commit_idx) with
    | Some (sha, _, _) -> sha | None -> "" in
  let files_for sha =
    Hashtbl.fold (fun (s', fb) _ acc ->
      if s' = sha && not (List.mem fb acc) then fb :: acc else acc
    ) s.data.git_links []
    |> List.sort String.compare in
  with_clickable_rows ui
    ~focus_set:(fun () -> Lwd.set s.focus Files)
    ~items_count:(fun () -> List.length (files_for (current_sha_peek ())))
    ~sel_var:s.file_idx
    ~on_scroll:(fun d ->
      let n = List.length (files_for (current_sha_peek ())) in
      let clamp i = max 0 (min (max 0 (n - 1)) i) in
      Lwd.set s.focus Files;
      Lwd.update (fun i -> clamp (i + d)) s.file_idx;
      Lwd.set s.link_idx 0;
      Lwd.set s.diff_scroll 0)

let links_panel s =
  let focused = Lwd.map (Lwd.get s.focus) ~f:(fun f -> f = Links) in
  let make =
    Lwd.map2
      (Lwd.map2 (current_links s) (Lwd.get s.link_idx)
         ~f:(fun links sel -> (links, sel)))
      focused
      ~f:(fun (links, sel) focused ~w ~h ->
        let items = List.map (fun (l : link) ->
          let sid = if String.length l.session_id >= 4
            then String.sub l.session_id 0 4 else l.session_id in
          Printf.sprintf "%s t%d e%d" sid l.turn_idx l.entry_idx) links in
        render_panel ~idx:4 ~title:"Links"
          ~focused ~items ~sel_idx:sel ~w ~h)
  in
  let ui = with_size_sensor ~focused make in
  let current_sha_peek () =
    match List.nth_opt (Lwd.peek s.commits) (Lwd.peek s.commit_idx) with
    | Some (sha, _, _) -> sha | None -> "" in
  let links_n () =
    let sha = current_sha_peek () in
    if sha = "" then 0
    else
      let files =
        Hashtbl.fold (fun (s', fb) _ acc ->
          if s' = sha && not (List.mem fb acc) then fb :: acc else acc
        ) s.data.git_links []
        |> List.sort String.compare in
      match List.nth_opt files (Lwd.peek s.file_idx) with
      | Some fb ->
        (match Hashtbl.find_opt s.data.git_links (sha, fb) with
         | Some l -> List.length l | None -> 0)
      | None -> 0 in
  with_clickable_rows ui
    ~focus_set:(fun () -> Lwd.set s.focus Links)
    ~items_count:links_n
    ~sel_var:s.link_idx
    ~on_scroll:(fun d ->
      let n = links_n () in
      let clamp i = max 0 (min (max 0 (n - 1)) i) in
      Lwd.set s.focus Links;
      Lwd.update (fun i -> clamp (i + d)) s.link_idx;
      Lwd.set s.diff_scroll 0)

(* -------- Key handling -------- *)

let cycle_focus ?(back=false) f =
  match back, f with
  | false, Branches -> Commits | false, Commits -> Files
  | false, Files -> Links    | false, Links -> Branches
  | true,  Branches -> Links   | true,  Commits -> Branches
  | true,  Files -> Commits  | true,  Links -> Files

let nav s delta =
  let clamp n i = max 0 (min (max 0 (n - 1)) i) in
  let current_sha_peek () =
    match List.nth_opt (Lwd.peek s.commits) (Lwd.peek s.commit_idx) with
    | Some (sha, _, _) -> sha | None -> "" in
  let files_for sha =
    Hashtbl.fold (fun (s', fb) _ acc ->
      if s' = sha && not (List.mem fb acc) then fb :: acc else acc
    ) s.data.git_links []
    |> List.sort String.compare in
  (* Purely update the index Lwd.vars. The drivers in the UI tree
     (commits_driver, content_driver) pick it up reactively. *)
  let _ = current_sha_peek, files_for in
  match Lwd.peek s.focus with
  | Branches ->
    let n = List.length s.data.branches in
    Lwd.update (fun i -> clamp n (i + delta)) s.branch_idx
  | Commits ->
    Lwd.update (fun i -> clamp (List.length (Lwd.peek s.commits)) (i + delta))
      s.commit_idx;
    Lwd.set s.file_idx 0;
    Lwd.set s.link_idx 0;
    Lwd.set s.diff_scroll 0
  | Files ->
    let files = files_for (current_sha_peek ()) in
    Lwd.update (fun i -> clamp (List.length files) (i + delta)) s.file_idx;
    Lwd.set s.link_idx 0;
    Lwd.set s.diff_scroll 0
  | Links ->
    let files = files_for (current_sha_peek ()) in
    let links_n =
      match List.nth_opt files (Lwd.peek s.file_idx) with
      | Some fb ->
        (match Hashtbl.find_opt s.data.git_links
                 (current_sha_peek (), fb) with
         | Some l -> List.length l | None -> 0)
      | None -> 0 in
    Lwd.update (fun i -> clamp links_n (i + delta)) s.link_idx

let hist_nav s delta =
  let n = List.length s.data.sessions in
  let before = Lwd.peek s.session_idx in
  Lwd.update (fun i -> max 0 (min (max 0 (n - 1)) (i + delta))) s.session_idx;
  let after = Lwd.peek s.session_idx in
  dbg (Printf.sprintf "hist_nav: delta=%d n=%d idx=%d -> %d" delta n before after);
  Lwd.set s.turn_idx 0;
  Lwd.set s.hist_scroll 0

let hist_turn_nav s delta =
  match List.nth_opt s.data.sessions (Lwd.peek s.session_idx) with
  | None -> ()
  | Some si ->
    let n = List.length si.turns in
    Lwd.update (fun i -> max 0 (min (max 0 (n - 1)) (i + delta))) s.turn_idx;
    Lwd.set s.hist_scroll 0

(* Defer every mutation off the Lwd sample pass. [async_update]
   schedules the [f] to run in an Lwt task, so Lwd sees a clean
   between-frames mutation. *)
(* Synchronous — Lwt.async was deferring updates to points where
   Lwd was still sampling, so invalidation never reached the root's
   on_invalidate. Calling directly mutates the var right away. *)
let async_update f = dbg "sync_update: running"; f ()

(* on_key: pattern-match on (mode, key), defer mutations via
   [async_update]. The Enter-on-Links case scrolls the diff_scroll
   to the line of the link's Edit/Write in the rendered body. *)
let on_key s (key, _mods) : U.may_handle =
  let kname = match key with
    | `ASCII c -> Printf.sprintf "ASCII %C" c
    | `Tab -> "Tab" | `Enter -> "Enter"
    | `Arrow `Up -> "Up" | `Arrow `Down -> "Down"
    | `Arrow `Left -> "Left" | `Arrow `Right -> "Right"
    | _ -> "?" in
  dbg (Printf.sprintf "key: %s (mode=%s)" kname
         (match Lwd.peek s.mode with
          | Git -> "Git" | History -> "History" | Search -> "Search"));
  let defer f = async_update f; `Handled in
  match Lwd.peek s.mode, key with
  (* Mode switches — except when typing in the search query field. *)
  | (Git | History), `ASCII 'q' -> Lwt.wakeup_later s.quit_u (); `Handled
  | Search, `ASCII 'q' when Lwd.peek s.search_focus = Results ->
    Lwt.wakeup_later s.quit_u (); `Handled
  | Git, `ASCII 'h' -> defer (fun () -> Lwd.set s.mode History)
  | Git, `ASCII 's' -> defer (fun () ->
      Lwd.set s.mode Search;
      Lwd.set s.search_focus Query)
  | History, `ASCII 'g' -> defer (fun () -> Lwd.set s.mode Git)
  | History, `ASCII 's' -> defer (fun () ->
      Lwd.set s.mode Search;
      Lwd.set s.search_focus Query)
  | Search, `ASCII 'g' when Lwd.peek s.search_focus = Results ->
    defer (fun () -> Lwd.set s.mode Git)
  | Search, `ASCII 'h' when Lwd.peek s.search_focus = Results ->
    defer (fun () -> Lwd.set s.mode History)
  (* Pressing [s] while in Results wipes the current search and
     puts focus back in the query field, ready for a new query. *)
  | Search, `ASCII 's' when Lwd.peek s.search_focus = Results ->
    defer (fun () ->
      Lwd.set s.search_query ("", 0);
      Lwd.set s.search_hits [];
      Lwd.set s.search_synthesis "";
      Lwd.set s.search_idx 0;
      Lwd.set s.search_scroll 0;
      Lwd.set s.search_view Overview;
      Lwd.set s.search_turn_offset 0;
      Lwd.set s.search_focus Query)
  (* ---- Query-focused (editing the search box) ---- *)
  | Search, `Enter when Lwd.peek s.search_focus = Query ->
    defer (fun () ->
      let q = String.trim (fst (Lwd.peek s.search_query)) in
      if q <> "" then s.run_search q)
  | Search, `Backspace when Lwd.peek s.search_focus = Query ->
    defer (fun () ->
      let (q, _c) = Lwd.peek s.search_query in
      let n = String.length q in
      let q' = if n = 0 then q else String.sub q 0 (n - 1) in
      Lwd.set s.search_query (q', String.length q'))
  | Search, `ASCII c when Lwd.peek s.search_focus = Query ->
    (* Any printable ASCII extends the query. Non-printables (< 32)
       are ignored — they'd be control characters. *)
    if Char.code c < 32 then `Unhandled
    else begin
      defer (fun () ->
        let (q, _cur) = Lwd.peek s.search_query in
        let q' = q ^ String.make 1 c in
        Lwd.set s.search_query (q', String.length q'))
    end
  | Search, `Arrow `Down when Lwd.peek s.search_focus = Query ->
    defer (fun () ->
      if Lwd.peek s.search_hits <> [] then
        Lwd.set s.search_focus Results)
  (* ---- Navigation-only keys (only meaningful in Results) ---- *)
  | Search, `Tab -> defer (fun () ->
      Lwd.update (function Query -> Results | Results -> Query)
        s.search_focus)
  | Search, `ASCII '/' when Lwd.peek s.search_focus = Results ->
    defer (fun () -> Lwd.set s.search_focus Query)
  | Search, `ASCII 'i' when Lwd.peek s.search_focus = Results ->
    defer (fun () -> Lwd.set s.search_focus Query)
  | Search, `Arrow `Down when Lwd.peek s.search_focus = Results ->
    defer (fun () ->
      let n = List.length (Lwd.peek s.search_hits) in
      let clamp i = max 0 (min (max 0 (n - 1)) i) in
      let cur = Lwd.peek s.search_idx in
      let new_idx = clamp (cur + 1) in
      dbg (Printf.sprintf "Search Down focus=Results idx=%d→%d n=%d"
             cur new_idx n);
      Lwd.set s.search_idx new_idx;
      Lwd.set s.search_scroll 0;
      dbg (Printf.sprintf "Search Down after: idx=%d" (Lwd.peek s.search_idx)))
  | Search, `Arrow `Up when Lwd.peek s.search_focus = Results ->
    defer (fun () ->
      let n = List.length (Lwd.peek s.search_hits) in
      let clamp i = max 0 (min (max 0 (n - 1)) i) in
      let cur = Lwd.peek s.search_idx in
      dbg (Printf.sprintf "Search Up focus=Results idx=%d n=%d" cur n);
      Lwd.update (fun i -> clamp (i - 1)) s.search_idx;
      Lwd.set s.search_scroll 0)
  | Search, `ASCII 'j' when Lwd.peek s.search_focus = Results ->
    defer (fun () ->
      match Lwd.peek s.search_view with
      | Overview ->
        let n = List.length (Lwd.peek s.search_hits) in
        let clamp i = max 0 (min (max 0 (n - 1)) i) in
        Lwd.update (fun i -> clamp (i + 1)) s.search_idx;
        Lwd.set s.search_scroll 0
      | Body -> Lwd.update (fun i -> i + 1) s.search_scroll)
  | Search, `ASCII 'k' when Lwd.peek s.search_focus = Results ->
    defer (fun () ->
      match Lwd.peek s.search_view with
      | Overview ->
        let n = List.length (Lwd.peek s.search_hits) in
        let clamp i = max 0 (min (max 0 (n - 1)) i) in
        Lwd.update (fun i -> clamp (i - 1)) s.search_idx;
        Lwd.set s.search_scroll 0
      | Body -> Lwd.update (fun i -> max 0 (i - 1)) s.search_scroll)
  | Search, `ASCII 'J' when Lwd.peek s.search_focus = Results ->
    defer (fun () -> Lwd.update (fun i -> i + 20) s.search_scroll)
  | Search, `ASCII 'K' when Lwd.peek s.search_focus = Results ->
    defer (fun () -> Lwd.update (fun i -> max 0 (i - 20)) s.search_scroll)
  | Search, `Enter when Lwd.peek s.search_focus = Results ->
    defer (fun () ->
      let v = Lwd.peek s.search_view in
      dbg (Printf.sprintf "Search Enter focus=Results view=%s"
             (match v with Overview -> "Overview" | Body -> "Body"));
      Lwd.update (function Overview -> Body | Body -> Overview) s.search_view;
      Lwd.set s.search_scroll 0)
  | Search, `Arrow `Right when Lwd.peek s.search_focus = Results ->
    defer (fun () ->
      Lwd.update (fun i -> i + 1) s.search_turn_offset;
      Lwd.set s.search_scroll 0)
  | Search, `Arrow `Left when Lwd.peek s.search_focus = Results ->
    defer (fun () ->
      Lwd.update (fun i -> i - 1) s.search_turn_offset;
      Lwd.set s.search_scroll 0)
  | Search, `Escape ->
    defer (fun () ->
      if Lwd.peek s.search_turn_offset <> 0 then begin
        Lwd.set s.search_turn_offset 0;
        Lwd.set s.search_scroll 0
      end
      else if Lwd.peek s.search_view = Body
      then (Lwd.set s.search_view Overview; Lwd.set s.search_scroll 0)
      else Lwd.set s.search_focus Query)
  (* Git mode: arrows move left panels, j/k scroll the diff/turn,
     J/K page, ←/→ step through turns when on Links, Enter toggles
     overview/body when on Links. *)
  | Git, `Tab -> defer (fun () ->
      Lwd.update cycle_focus s.focus;
      Lwd.set s.diff_scroll 0;
      Lwd.set s.link_turn_offset 0)
  | Git, `Arrow `Down -> defer (fun () ->
      nav s 1; Lwd.set s.diff_scroll 0; Lwd.set s.link_turn_offset 0)
  | Git, `Arrow `Up -> defer (fun () ->
      nav s (-1); Lwd.set s.diff_scroll 0; Lwd.set s.link_turn_offset 0)
  | Git, `Arrow `Right
    when (Lwd.peek s.focus = Links) ->
    defer (fun () ->
      Lwd.update (fun i -> i + 1) s.link_turn_offset;
      Lwd.set s.diff_scroll 0)
  | Git, `Arrow `Left
    when (Lwd.peek s.focus = Links) ->
    defer (fun () ->
      Lwd.update (fun i -> i - 1) s.link_turn_offset;
      Lwd.set s.diff_scroll 0)
  | Git, `Enter
    when (Lwd.peek s.focus = Links) ->
    defer (fun () ->
      Lwd.update (function Overview -> Body | Body -> Overview) s.hist_view;
      Lwd.set s.diff_scroll 0)
  | Git, `Escape
    when (Lwd.peek s.focus = Links) ->
    defer (fun () ->
      Lwd.set s.link_turn_offset 0;
      Lwd.set s.diff_scroll 0)
  | Git, `ASCII 'j' ->
    defer (fun () -> Lwd.update (fun i -> i + 1) s.diff_scroll)
  | Git, `ASCII 'k' ->
    defer (fun () -> Lwd.update (fun i -> max 0 (i - 1)) s.diff_scroll)
  | Git, `ASCII 'J' ->
    defer (fun () -> Lwd.update (fun i -> i + 20) s.diff_scroll)
  | Git, `ASCII 'K' ->
    defer (fun () -> Lwd.update (fun i -> max 0 (i - 20)) s.diff_scroll)
  | Git, `ASCII '[' ->
    defer (fun () -> Lwd.update (fun i -> max 0 (i - 1)) s.diff_scroll)
  | Git, `ASCII ']' ->
    defer (fun () -> Lwd.update (fun i -> i + 1) s.diff_scroll)
  (* History mode *)
  | History, `Enter ->
    defer (fun () ->
      Lwd.update (function Overview -> Body | Body -> Overview) s.hist_view;
      Lwd.set s.hist_scroll 0)
  | History, `Escape ->
    defer (fun () -> Lwd.set s.hist_view Overview; Lwd.set s.hist_scroll 0)
  | History, `Tab ->
    defer (fun () ->
      Lwd.update (function Sessions -> Turns | Turns -> Sessions) s.hist_focus)
  (* In Overview: j/k navigates session/turn. In Body: j/k scrolls
     the text, J/K page-scrolls. Arrow keys always navigate. *)
  (* Arrows always navigate the list (sessions/turns), in both
     Overview and Body view — matches git-links behaviour where
     ↑/↓ move selection and the right panel auto-updates. Use
     j/k / J/K to scroll the body. *)
  | History, `Arrow `Down ->
    defer (fun () ->
      match Lwd.peek s.hist_focus with
      | Sessions -> hist_nav s 1
      | Turns -> hist_turn_nav s 1)
  | History, `Arrow `Up ->
    defer (fun () ->
      match Lwd.peek s.hist_focus with
      | Sessions -> hist_nav s (-1)
      | Turns -> hist_turn_nav s (-1))
  | History, `ASCII 'j' ->
    defer (fun () ->
      match Lwd.peek s.hist_view with
      | Overview ->
        (match Lwd.peek s.hist_focus with
         | Sessions -> hist_nav s 1
         | Turns -> hist_turn_nav s 1)
      | Body -> Lwd.update (fun i -> i + 1) s.hist_scroll)
  | History, `ASCII 'k' ->
    defer (fun () ->
      match Lwd.peek s.hist_view with
      | Overview ->
        (match Lwd.peek s.hist_focus with
         | Sessions -> hist_nav s (-1)
         | Turns -> hist_turn_nav s (-1))
      | Body -> Lwd.update (fun i -> max 0 (i - 1)) s.hist_scroll)
  | History, `ASCII 'J' ->
    defer (fun () ->
      if Lwd.peek s.hist_view = Body then
        Lwd.update (fun i -> i + 20) s.hist_scroll)
  | History, `ASCII 'K' ->
    defer (fun () ->
      if Lwd.peek s.hist_view = Body then
        Lwd.update (fun i -> max 0 (i - 20)) s.hist_scroll)
  | History, `ASCII '[' ->
    defer (fun () -> Lwd.update (fun i -> max 0 (i - 1)) s.hist_scroll)
  | History, `ASCII ']' ->
    defer (fun () -> Lwd.update (fun i -> i + 1) s.hist_scroll)
  | _ -> `Unhandled

(* -------- Root UI -------- *)

(* ---------- History mode ---------- *)

(* Strip newlines/controls — they make notty's I.string misbehave. *)
let sanitize s =
  String.map (fun c ->
    if c = '\n' || c = '\r' || c = '\t' then ' '
    else if Char.code c < 32 then '?' else c) s

let sessions_panel s =
  let focused = Lwd.map (Lwd.get s.hist_focus) ~f:(fun f -> f = Sessions) in
  let make =
    Lwd.map2 (Lwd.get s.session_idx) focused ~f:(fun sel focused ~w ~h ->
      let items = List.map (fun si ->
        let tm = Unix.gmtime si.started_at in
        let date = Printf.sprintf
          "%04d-%02d-%02d %02d:%02d"
          (tm.tm_year + 1900) (tm.tm_mon + 1) tm.tm_mday
          tm.tm_hour tm.tm_min in
        Printf.sprintf "%s  %s" date (sanitize si.first_prompt)
      ) s.data.sessions in
      render_panel ~idx:1 ~title:"Sessions" ~focused
        ~items ~sel_idx:sel ~w ~h)
  in
  let ui = with_size_sensor ~focused make in
  with_clickable_rows ui
    ~focus_set:(fun () -> Lwd.set s.hist_focus Sessions)
    ~items_count:(fun () -> List.length s.data.sessions)
    ~sel_var:s.session_idx
    ~on_scroll:(fun d ->
      Lwd.set s.hist_focus Sessions;
      hist_nav s d)

let turns_panel s =
  let focused = Lwd.map (Lwd.get s.hist_focus) ~f:(fun f -> f = Turns) in
  let turns_lwd = Lwd.map (Lwd.get s.session_idx) ~f:(fun sidx ->
    match List.nth_opt s.data.sessions sidx with
    | None -> []
    | Some si -> si.turns) in
  let make =
    Lwd.map2
      (Lwd.map2 turns_lwd (Lwd.get s.turn_idx)
         ~f:(fun turns sel -> (turns, sel)))
      focused
      ~f:(fun (turns, sel) focused ~w ~h ->
        let items = List.map (fun (t : turn_info) ->
          (* Prefer the per-turn summary over the raw user prompt —
             the summary is a more useful at-a-glance label. Fall
             back to the prompt when no summary is available yet. *)
          let text =
            let s = String.trim t.t_summary in
            if s <> "" then s else String.trim t.t_prompt in
          let preview = sanitize text in
          let preview = if String.length preview > 60
            then String.sub preview 0 60 ^ "…" else preview in
          Printf.sprintf "t%d  %s" t.t_turn_idx preview) turns in
        render_panel ~idx:2 ~title:"Turns" ~focused
          ~items ~sel_idx:sel ~w ~h)
  in
  let ui = with_size_sensor ~focused make in
  let turns_count () =
    match List.nth_opt s.data.sessions (Lwd.peek s.session_idx) with
    | Some si -> List.length si.turns | None -> 0 in
  with_clickable_rows ui
    ~focus_set:(fun () -> Lwd.set s.hist_focus Turns)
    ~items_count:turns_count
    ~sel_var:s.turn_idx
    ~on_scroll:(fun d ->
      Lwd.set s.hist_focus Turns;
      hist_turn_nav s d)


(* Return a list of (attr, line) pairs — each content block's lines
   get the legacy colour for that kind. *)
let render_full_turn_styled ?highlight_edit_key ~project_dir ~session_id ~turn_idx () =
  let lines : (Notty.A.t * string) list ref = ref [] in
  let push attr s =
    String.split_on_char '\n' s
    |> List.iter (fun l -> lines := (attr, sanitize l) :: !lines) in
  (try
    let jsonl_dir =
      Urme_search.Jsonl_reader.find_jsonl_dir ~project_dir in
    let filepath = Filename.concat jsonl_dir (session_id ^ ".jsonl") in
    if Sys.file_exists filepath then begin
      let interactions =
        Urme_search.Jsonl_reader.parse_interactions ~filepath in
      match List.find_opt (fun (i : Urme_core.Types.interaction) ->
        i.index = turn_idx) interactions with
      | None -> ()
      | Some i ->
        let raw_lines =
          Urme_search.Jsonl_reader.read_all_lines filepath in
        (* Claude-Code-style palette: green bullets for assistant,
           yellow for user prompt, indented branches for sub-items. *)
        let a_bullet = A.(fg lightgreen ++ st bold) in
        let a_ass    = A.(fg c_fg_main) in
        let a_user   = A.(fg c_user ++ st bold) in
        let a_user_b = A.(fg (rgb_888 ~r:255 ~g:230 ~b:180)) in
        let a_think  = A.(fg c_thinking ++ st italic) in
        let a_branch = A.(fg (rgb_888 ~r:200 ~g:200 ~b:200)) in
        let a_tool   = A.(fg c_tool_name ++ st bold) in
        let a_add    = A.(fg (rgb_888 ~r:255 ~g:255 ~b:255)
                          ++ bg (rgb_888 ~r:15 ~g:70 ~b:15) ++ st bold) in
        let a_del    = A.(fg (rgb_888 ~r:255 ~g:255 ~b:255)
                          ++ bg (rgb_888 ~r:90 ~g:20 ~b:20) ++ st bold) in
        push a_user (Printf.sprintf "●  turn %d — %s" i.index i.timestamp);
        push a_user_b "";
        let user_txt = String.trim i.user_text in
        push a_user_b ("> " ^ user_txt);
        push a_user_b "";
        let open Yojson.Safe.Util in
        List.iteri (fun ln raw ->
          if ln < i.line_start || ln > i.line_end then ()
          else
            try
              let j = Yojson.Safe.from_string raw in
              let typ = j |> member "type" |> to_string_option in
              match typ with
              | Some "assistant" ->
                let blocks = j |> member "message" |> member "content"
                  |> (fun v -> try to_list v with _ -> []) in
                List.iter (fun b ->
                  match b |> member "type" |> to_string_option with
                  | Some "thinking" ->
                    let t = b |> member "thinking" |> to_string_option
                      |> Option.value ~default:"" in
                    if t <> "" then begin
                      push a_think "";
                      push a_think "✻ Thinking";
                      String.split_on_char '\n' t
                      |> List.iter (fun l -> push a_think ("  " ^ l))
                    end
                  | Some "text" ->
                    let t = b |> member "text" |> to_string_option
                      |> Option.value ~default:"" in
                    if t <> "" then begin
                      push a_ass "";
                      let parts = String.split_on_char '\n' t in
                      List.iteri (fun idx l ->
                        if idx = 0 then push a_bullet ("● " ^ l)
                        else push a_ass ("  " ^ l)) parts
                    end
                  | Some "tool_use" ->
                    let name = b |> member "name" |> to_string_option
                      |> Option.value ~default:"" in
                    let inp = try b |> member "input" with _ -> `Null in
                    let old_s = inp |> member "old_string" |> to_string_option
                      |> Option.value ~default:"" in
                    let new_s = inp |> member "new_string" |> to_string_option
                      |> Option.value ~default:"" in
                    let file_path_s = inp |> member "file_path" |> to_string_option
                      |> Option.value ~default:"" in
                    let write_content = inp |> member "content" |> to_string_option
                      |> Option.value ~default:"" in
                    let is_match =
                      match highlight_edit_key, name with
                      | Some ek, "Edit" ->
                        let fb = Filename.basename file_path_s in
                        ek = Urme_engine.Git_link_types.make_edit_key
                               ~file_base:fb ~old_string:old_s ~new_string:new_s
                      | Some ek, "Write" ->
                        let fb = Filename.basename file_path_s in
                        ek = Urme_engine.Git_link_types.make_edit_key
                               ~file_base:fb ~old_string:"" ~new_string:write_content
                      | _ -> false in
                    let relpath p =
                      if p = "" then ""
                      else if String.length p > String.length project_dir
                           && String.sub p 0 (String.length project_dir)
                              = project_dir
                      then
                        let s = String.sub p (String.length project_dir)
                                  (String.length p - String.length project_dir) in
                        if String.length s > 0 && s.[0] = '/'
                        then String.sub s 1 (String.length s - 1) else s
                      else p in
                    let arg = match name with
                      | "Bash" ->
                        let c = inp |> member "command" |> to_string_option
                          |> Option.value ~default:"" in
                        if String.length c > 80 then String.sub c 0 77 ^ "..." else c
                      | "Edit" | "Write" | "Read" | "MultiEdit" ->
                        relpath file_path_s
                      | "Grep" | "Glob" ->
                        inp |> member "pattern" |> to_string_option
                        |> Option.value ~default:""
                      | _ -> "" in
                    push a_tool "";
                    if is_match then
                      push A.(fg lightyellow ++ st bold)
                        "  »» THIS LINK'S EDIT »»";
                    (* Header: "● ToolName(arg)" *)
                    push a_tool (Printf.sprintf "● %s(%s)" name arg);
                    (* Summary branch for Edit/Write showing +/- counts *)
                    (match name with
                     | "Edit" ->
                       let n_old =
                         if old_s = "" then 0
                         else List.length (String.split_on_char '\n' old_s) in
                       let n_new =
                         if new_s = "" then 0
                         else List.length (String.split_on_char '\n' new_s) in
                       push a_branch
                         (Printf.sprintf "  ⎿  %d -, %d +" n_old n_new);
                       let emit s attr prefix =
                         String.split_on_char '\n' s
                         |> List.iteri (fun idx l ->
                           let prefix_txt =
                             Printf.sprintf "     %4d %s " (idx + 1) prefix in
                           push attr (prefix_txt ^ l)) in
                       emit old_s a_del "-";
                       emit new_s a_add "+"
                     | "Write" ->
                       let lines = String.split_on_char '\n' write_content in
                       push a_branch
                         (Printf.sprintf "  ⎿  wrote %d lines"
                            (List.length lines));
                       List.iteri (fun idx l ->
                         if idx < 20 then
                           let prefix_txt =
                             Printf.sprintf "     %4d + " (idx + 1) in
                           push a_add (prefix_txt ^ l)) lines;
                       if List.length lines > 20 then
                         push a_branch
                           (Printf.sprintf "     ... (%d more)"
                              (List.length lines - 20))
                     | _ -> ())
                  | _ -> ()) blocks
              | Some "user" ->
                let c = j |> member "message" |> member "content" in
                (match c with
                 | `List items ->
                   List.iter (fun it ->
                     if it |> member "type" |> to_string_option = Some "tool_result"
                     then
                       let content = it |> member "content" in
                       let text = match content with
                         | `String s -> s
                         | `List its ->
                           List.filter_map (fun x ->
                             if x |> member "type" |> to_string_option = Some "text"
                             then x |> member "text" |> to_string_option
                             else None) its |> String.concat "\n"
                         | _ -> "" in
                       if text <> "" then
                         let t = if String.length text > 400
                                 then String.sub text 0 400 ^ "…" else text in
                         let parts = String.split_on_char '\n' t in
                         List.iteri (fun idx l ->
                           if idx = 0 then push a_branch ("  ⎿  " ^ l)
                           else push a_branch ("     " ^ l)) parts
                  ) items
                 | _ -> ())
              | _ -> ()
            with _ -> ()
        ) raw_lines
    end
   with _ -> ());
  List.rev !lines

(* Keep the plain string version as a fallback. *)
let render_full_turn ~project_dir ~session_id ~turn_idx =
  try
    let jsonl_dir =
      Urme_search.Jsonl_reader.find_jsonl_dir ~project_dir in
    let filepath = Filename.concat jsonl_dir (session_id ^ ".jsonl") in
    if not (Sys.file_exists filepath) then None
    else
      let interactions =
        Urme_search.Jsonl_reader.parse_interactions ~filepath in
      match List.find_opt (fun (i : Urme_core.Types.interaction) ->
        i.index = turn_idx) interactions with
      | None -> None
      | Some i ->
        let lines = Urme_search.Jsonl_reader.read_all_lines filepath in
        let buf = Buffer.create 4096 in
        Buffer.add_string buf
          (Printf.sprintf "# turn %d — %s\n" i.index i.timestamp);
        Buffer.add_string buf "\n> user:\n";
        Buffer.add_string buf (String.trim i.user_text);
        Buffer.add_string buf "\n";
        let open Yojson.Safe.Util in
        List.iteri (fun ln raw ->
          if ln < i.line_start || ln > i.line_end then ()
          else begin
            try
              let j = Yojson.Safe.from_string raw in
              let typ = j |> member "type" |> to_string_option in
              match typ with
              | Some "assistant" ->
                let blocks = j |> member "message" |> member "content"
                  |> (fun v -> try to_list v with _ -> []) in
                List.iter (fun b ->
                  match b |> member "type" |> to_string_option with
                  | Some "thinking" ->
                    let t = b |> member "thinking" |> to_string_option
                      |> Option.value ~default:"" in
                    if t <> "" then begin
                      Buffer.add_string buf "\n> thinking:\n";
                      Buffer.add_string buf t;
                      Buffer.add_char buf '\n';
                    end
                  | Some "text" ->
                    let t = b |> member "text" |> to_string_option
                      |> Option.value ~default:"" in
                    if t <> "" then begin
                      Buffer.add_string buf "\n> assistant:\n";
                      Buffer.add_string buf t;
                      Buffer.add_char buf '\n';
                    end
                  | Some "tool_use" ->
                    let name = b |> member "name" |> to_string_option
                      |> Option.value ~default:"" in
                    let input_s =
                      try Yojson.Safe.to_string (b |> member "input")
                      with _ -> "" in
                    Buffer.add_string buf
                      (Printf.sprintf "\n> tool_use [%s]: %s\n" name input_s)
                  | _ -> ()) blocks
              | Some "user" ->
                let c = j |> member "message" |> member "content" in
                (match c with
                 | `List items ->
                   List.iter (fun it ->
                     match it |> member "type" |> to_string_option with
                     | Some "tool_result" ->
                       let content = it |> member "content" in
                       let text = match content with
                         | `String s -> s
                         | `List items ->
                           List.filter_map (fun x ->
                             match x |> member "type" |> to_string_option with
                             | Some "text" ->
                               x |> member "text" |> to_string_option
                             | _ -> None) items
                           |> String.concat "\n"
                         | _ -> "" in
                       if text <> "" then begin
                         Buffer.add_string buf "\n> tool_result:\n";
                         Buffer.add_string buf text;
                         Buffer.add_char buf '\n';
                       end
                     | _ -> ()) items
                 | _ -> ())
              | _ -> ()
            with _ -> ()
          end
        ) lines;
        Some (Buffer.contents buf)
  with _ -> None

(* Compact overview: summary + tags + files + commands + tool-use
   list + user excerpt. No full conversation text — press Enter to
   switch to Body view for that. *)
let render_turn_overview ~project_dir (t : turn_info) ~session_id =
  let tm = Unix.gmtime t.t_timestamp in
  let date = Printf.sprintf "%04d-%02d-%02d %02d:%02d"
    (tm.tm_year + 1900) (tm.tm_mon + 1) tm.tm_mday
    tm.tm_hour tm.tm_min in
  let sid = if String.length session_id >= 8
    then String.sub session_id 0 8 else session_id in
  let buf = Buffer.create 512 in
  Buffer.add_string buf
    (Printf.sprintf "session: %s  step %d  %s\n" sid t.t_turn_idx date);
  if t.t_summary <> "" then begin
    Buffer.add_string buf "\nSummary:\n  ";
    Buffer.add_string buf t.t_summary;
    Buffer.add_char buf '\n';
  end;
  if t.t_files <> [] then begin
    Buffer.add_string buf (Printf.sprintf "\nFiles (%d):\n" (List.length t.t_files));
    List.iter (fun f -> Buffer.add_string buf (Printf.sprintf "  %s\n" f)) t.t_files;
  end;
  if t.t_commands <> [] then begin
    Buffer.add_string buf (Printf.sprintf "\nCommands (%d):\n" (List.length t.t_commands));
    List.iter (fun c ->
      let c = if String.length c > 100 then String.sub c 0 100 ^ "…" else c in
      Buffer.add_string buf (Printf.sprintf "  $ %s\n" c)) t.t_commands;
  end;
  (* tool_use list from JSONL range *)
  (try
     let jsonl_dir =
       Urme_search.Jsonl_reader.find_jsonl_dir ~project_dir in
     let filepath = Filename.concat jsonl_dir (session_id ^ ".jsonl") in
     if Sys.file_exists filepath then begin
       let interactions =
         Urme_search.Jsonl_reader.parse_interactions ~filepath in
       match List.find_opt (fun (i : Urme_core.Types.interaction) ->
         i.index = t.t_turn_idx) interactions with
       | None -> ()
       | Some i ->
         let lines = Urme_search.Jsonl_reader.read_all_lines filepath in
         let open Yojson.Safe.Util in
         let tool_uses = ref [] in
         List.iteri (fun ln raw ->
           if ln >= i.line_start && ln <= i.line_end then
             try
               let j = Yojson.Safe.from_string raw in
               if j |> member "type" |> to_string_option = Some "assistant" then
                 let blocks = j |> member "message" |> member "content"
                   |> (fun v -> try to_list v with _ -> []) in
                 List.iter (fun b ->
                   if b |> member "type" |> to_string_option = Some "tool_use" then
                     let name = b |> member "name" |> to_string_option
                       |> Option.value ~default:"" in
                     let preview =
                       let inp = try b |> member "input" with _ -> `Null in
                       match name with
                       | "Bash" ->
                         "$ " ^ (inp |> member "command" |> to_string_option
                                 |> Option.value ~default:"")
                       | "Edit" | "Write" | "Read" ->
                         inp |> member "file_path" |> to_string_option
                         |> Option.value ~default:""
                       | "Grep" ->
                         "/" ^ (inp |> member "pattern" |> to_string_option
                                |> Option.value ~default:"") ^ "/"
                       | "Glob" ->
                         inp |> member "pattern" |> to_string_option
                         |> Option.value ~default:""
                       | "Task" ->
                         inp |> member "description" |> to_string_option
                         |> Option.value ~default:""
                       | _ -> "" in
                     tool_uses := (name, preview) :: !tool_uses) blocks
             with _ -> ()
         ) lines;
         let uses = List.rev !tool_uses in
         if uses <> [] then begin
           Buffer.add_string buf
             (Printf.sprintf "\nActions (%d):\n" (List.length uses));
           List.iter (fun (n, p) ->
             let p = if String.length p > 100
               then String.sub p 0 100 ^ "…" else p in
             Buffer.add_string buf (Printf.sprintf "  [%s] %s\n" n p)) uses
         end
     end
   with _ -> ());
  Buffer.add_string buf "\nUser message (excerpt):\n  ";
  let u = String.trim t.t_prompt in
  let u = if String.length u > 500 then String.sub u 0 500 ^ "…" else u in
  Buffer.add_string buf u;
  Buffer.add_char buf '\n';
  Buffer.add_string buf "\n(press Enter for full turn body)\n";
  Buffer.contents buf

let current_turn_text s : string Lwd.t =
  Lwd.map2
    (Lwd.map2 (Lwd.get s.session_idx) (Lwd.get s.turn_idx)
       ~f:(fun sidx tidx -> (sidx, tidx)))
    (Lwd.get s.hist_view)
    ~f:(fun (sidx, tidx) view ->
      match List.nth_opt s.data.sessions sidx with
      | None -> "(no session)"
      | Some si ->
        match List.nth_opt si.turns tidx with
        | None -> "(no turn)"
        | Some t ->
          match view with
          | Overview ->
            render_turn_overview ~project_dir:s.data.project_dir
              ~session_id:si.session_id t
          | Body ->
            (match render_full_turn
                     ~project_dir:s.data.project_dir
                     ~session_id:si.session_id
                     ~turn_idx:t.t_turn_idx with
             | Some s -> s
             | None -> String.trim t.t_prompt))

let _legacy_current_turn_text s =
  Lwd.map2 (Lwd.get s.session_idx) (Lwd.get s.turn_idx)
    ~f:(fun sidx tidx ->
      match List.nth_opt s.data.sessions sidx with
      | None -> "(no session)"
      | Some si ->
        match List.nth_opt si.turns tidx with
        | None -> "(no turn)"
        | Some t ->
          match render_full_turn
                  ~project_dir:s.data.project_dir
                  ~session_id:si.session_id
                  ~turn_idx:t.t_turn_idx with
          | Some s -> s
          | None ->
            (* fallback to the DB columns *)
            let tm = Unix.gmtime t.t_timestamp in
            let date = Printf.sprintf "%04d-%02d-%02d %02d:%02d"
              (tm.tm_year + 1900) (tm.tm_mon + 1) tm.tm_mday
              tm.tm_hour tm.tm_min in
            Printf.sprintf "# turn %d — %s\n\n> user:\n%s\n"
              t.t_turn_idx date (String.trim t.t_prompt))

let render_session_stats (si : session_info) : (Notty.A.t * string) list =
  let n_turns = List.length si.turns in
  let total_prompt_chars =
    List.fold_left (fun acc t -> acc + String.length t.t_prompt) 0 si.turns in
  let all_files =
    List.fold_left (fun acc t ->
      List.fold_left (fun a f -> if List.mem f a then a else f :: a) acc t.t_files
    ) [] si.turns in
  let n_commands =
    List.fold_left (fun acc t -> acc + List.length t.t_commands) 0 si.turns in
  let date_of ts =
    let tm = Unix.gmtime ts in
    Printf.sprintf "%04d-%02d-%02d %02d:%02d"
      (tm.tm_year + 1900) (tm.tm_mon + 1) tm.tm_mday
      tm.tm_hour tm.tm_min in
  let first_ts = si.started_at in
  let last_ts = match List.rev si.turns with
    | [] -> first_ts
    | t :: _ -> t.t_timestamp in
  let sid = if String.length si.session_id >= 8
    then String.sub si.session_id 0 8 else si.session_id in
  let kv key v =
    A.(fg c_tool_name ++ st bold), Printf.sprintf "  %-16s %s" (key ^ ":") v in
  let heading s = A.(fg c_hunk ++ st bold), s in
  [ heading (Printf.sprintf "# session %s" sid);
    kv "started" (date_of first_ts);
    kv "ended"   (date_of last_ts);
    kv "turns" (string_of_int n_turns);
    kv "commands" (string_of_int n_commands);
    kv "prompt chars" (string_of_int total_prompt_chars);
    kv "unique files" (string_of_int (List.length all_files));
    heading "";
    heading "Files touched:" ]
  @ List.map (fun f -> A.(fg c_fg_main), "  " ^ f)
      (List.sort String.compare all_files)
  @ [ heading "";
      heading "First prompt:";
      (A.(fg c_user), "  " ^ String.trim si.first_prompt) ]

(* Body mode renders the styled per-block list; Overview mode uses
   the plain-text compact summary. *)
let current_turn_styled s : (Notty.A.t * string) list Lwd.t =
  let idxs = Lwd.map2 (Lwd.get s.session_idx) (Lwd.get s.turn_idx)
    ~f:(fun sidx tidx -> (sidx, tidx)) in
  let modes = Lwd.map2 (Lwd.get s.hist_view) (Lwd.get s.hist_focus)
    ~f:(fun v f -> (v, f)) in
  Lwd.map2 idxs modes ~f:(fun (sidx, tidx) (view, focus) ->
    match List.nth_opt s.data.sessions sidx with
    | None -> [(A.empty, "(no session)")]
    | Some si ->
      (* On the Sessions panel: show session-level stats, not a turn. *)
      if focus = Sessions then render_session_stats si
      else
        match List.nth_opt si.turns tidx with
        | None -> [(A.empty, "(no turn)")]
        | Some t ->
          match view with
          | Body ->
            render_full_turn_styled
              ~project_dir:s.data.project_dir
              ~session_id:si.session_id
              ~turn_idx:t.t_turn_idx ()
          | Overview ->
            let txt = render_turn_overview
              ~project_dir:s.data.project_dir
              ~session_id:si.session_id t in
            String.split_on_char '\n' txt
            |> List.map (fun l ->
              let attr =
                if String.length l > 0 && l.[0] = '#' then
                  A.(fg c_hunk ++ st bold)
                else if String.length l >= 8 && String.sub l 0 8 = "Summary:"
                then A.(fg c_assistant ++ st bold)
                else if String.length l >= 6 && String.sub l 0 6 = "Files "
                     || String.length l >= 9 && String.sub l 0 9 = "Commands "
                     || String.length l >= 8 && String.sub l 0 8 = "Actions "
                     || String.length l >= 19 && String.sub l 0 19 = "User message (excer"
                then A.(fg c_tool_name ++ st bold)
                else if String.length l > 0 && l.[0] = ' '
                then A.(fg c_fg_main)
                else A.(fg c_assistant) in
              (attr, l)))

(* Render a full size-aware boxed panel for scrollable styled lines
   (used by History body, Link panel, Diff panel). Borders follow the
   same style as the left panels. *)
let styled_panel ?(idx=5) ?(title_text="") ?(focused=Lwd.pure false)
    lines_lwd scroll_var =
  let w_var = Lwd.var 80 in
  let h_var = Lwd.var 20 in
  let sensor : U.size_sensor = fun ~w ~h ->
    let ow = Lwd.peek w_var and oh = Lwd.peek h_var in
    if ow <> w || oh <> h then
      Lwt.async (fun () ->
        if Lwd.peek w_var <> w then Lwd.set w_var w;
        if Lwd.peek h_var <> h then Lwd.set h_var h;
        Lwt.return_unit) in
  let size = Lwd.pair (Lwd.get w_var) (Lwd.get h_var) in
  let body = Lwd.map2 (Lwd.pair size focused)
    (Lwd.map2 lines_lwd (Lwd.get scroll_var)
       ~f:(fun a b -> (a, b)))
    ~f:(fun ((w, h), foc) (lines, scroll) ->
      if w < 4 || h < 3 then U.empty
      else
        let border_attr =
          if foc then A.(fg lightcyan ++ bg c_bg ++ st bold)
          else A.(fg (gray 12) ++ bg c_bg) in
        let title_attr = A.(fg c_hunk ++ bg c_bg ++ st bold) in
        let inner_w = max 1 (w - 2) in
        let body_h = max 0 (h - 2) in
        let rec drop n xs = if n <= 0 then xs
          else match xs with [] -> [] | _ :: r -> drop (n - 1) r in
        let visible = drop scroll lines in
        let total = List.length lines in
        let label = Printf.sprintf "[%d]─%s" idx title_text in
        let lbl_cells = display_width label in
        let fill_n = max 0 (inner_w - lbl_cells - 1) in
        let top = I.hcat [
          I.string border_attr "┌─";
          I.string title_attr label;
          dash_img border_attr fill_n;
          I.string border_attr "┐";
        ] in
        let bot = I.hcat [
          I.string border_attr "└";
          dash_img border_attr inner_w;
          I.string border_attr "┘";
        ] in
        let scroll_info =
          Printf.sprintf " line %d/%d " (min total (scroll + 1)) total in
        let info_row =
          let pad_n = max 0 (inner_w - display_width scroll_info) in
          I.hcat [
            I.string border_attr "│";
            I.char A.(bg c_bg) ' ' pad_n 1;
            I.string title_attr scroll_info;
            I.string border_attr "│";
          ] in
        let make_row (attr, l) =
          let content = pad_cells ("  " ^ sanitize l) inner_w in
          I.hcat [
            I.string border_attr "│";
            I.string attr content;
            I.string border_attr "│";
          ] in
        let rows = List.map make_row
          (List.filteri (fun i _ -> i < body_h - 1) visible) in
        let filler =
          I.hcat [
            I.string border_attr "│";
            I.char A.(bg c_bg) ' ' inner_w 1;
            I.string border_attr "│";
          ] in
        let pads = List.init
          (max 0 (body_h - 1 - List.length rows)) (fun _ -> filler) in
        U.atom (I.vcat (top :: info_row :: rows @ pads @ [bot]))) in
  Lwd.map body ~f:(fun ui ->
    U.size_sensor sensor
      (U.resize ~w:0 ~h:0 ~sw:1 ~sh:1 ui))

let history_body_panel s =
  let focused = Lwd.pure true in
  let ui =
    styled_panel ~idx:3 ~focused ~title_text:"Turn"
      (current_turn_styled s) s.hist_scroll in
  with_mouse ui
    ~on_click:(fun () -> ())
    ~on_scroll:(fun d ->
      Lwd.update (fun i -> max 0 (i + d)) s.hist_scroll)

(* Diff panel — reuses [styled_panel] with diff-colour-attr per line. *)
let diff_panel s =
  let focused = Lwd.map (Lwd.get s.focus) ~f:(fun f -> f = Links) in
  let styled_lines = Lwd.map (Lwd.get s.current_diff) ~f:(fun text ->
    let attr_for l =
      if String.length l = 0 then A.(fg c_fg_main)
      else match l.[0] with
        | '+' -> A.(fg c_diff_add)
        | '-' -> A.(fg c_diff_del)
        | '@' -> A.(fg c_hunk ++ st bold)
        | _   -> A.(fg c_fg_main) in
    String.split_on_char '\n' text
    |> List.map (fun l -> (attr_for l, l))) in
  styled_panel ~idx:5 ~focused ~title_text:"Diff"
    styled_lines s.diff_scroll

(* Link-focused right panel: pulls the linked turn's content in
   Overview/Body (styled), scrollable, with ←/→ stepping through
   surrounding turns for context. *)
let link_context s : (string * int) option Lwd.t =
  Lwd.map2 (current_links s)
    (Lwd.map2 (Lwd.get s.link_idx) (Lwd.get s.link_turn_offset)
       ~f:(fun a b -> (a, b)))
    ~f:(fun links (lidx, offset) ->
      let sorted = List.sort (fun (a : link) b ->
        let c = Int.compare a.turn_idx b.turn_idx in
        if c <> 0 then c else Int.compare a.entry_idx b.entry_idx) links in
      match List.nth_opt sorted lidx with
      | Some l when l.session_id <> "human" && l.session_id <> "" ->
        Some (l.session_id, l.turn_idx + offset)
      | _ -> None)

(* Link overview = history-style turn summary + the link's edit.
   [offset] is the ←/→ context offset; when non-zero we're viewing
   a neighbouring turn and the link's specific edit isn't shown
   (it belongs to a different turn). *)
let render_link_overview ?(offset=0) (link : link) s =
  let effective_turn = link.turn_idx + offset in
  let show_link_edit = offset = 0 in
  let heading txt = (A.(fg c_hunk ++ st bold), txt) in
  let plain txt = (A.(fg c_fg_main), txt) in
  let rows = ref [] in
  let push x = rows := x :: !rows in
  let sid = link.session_id in
  (* Look up turn_info so we can reuse [render_turn_overview]. *)
  let turn_info =
    match List.find_opt (fun si -> si.session_id = sid) s.data.sessions with
    | None -> None
    | Some si ->
      List.find_opt (fun t -> t.t_turn_idx = effective_turn) si.turns
  in
  (match turn_info with
   | Some t ->
     let txt = render_turn_overview
       ~project_dir:s.data.project_dir ~session_id:sid t in
     String.split_on_char '\n' txt
     |> List.iter (fun l ->
       let attr =
         if String.length l > 0 && l.[0] = '#' then
           A.(fg c_hunk ++ st bold)
         else if String.length l >= 8 && String.sub l 0 8 = "Summary:"
         then A.(fg c_assistant ++ st bold)
         else if (String.length l >= 6 && String.sub l 0 6 = "Files ")
              || (String.length l >= 9 && String.sub l 0 9 = "Commands ")
              || (String.length l >= 8 && String.sub l 0 8 = "Actions ")
              || (String.length l >= 19 && String.sub l 0 19 = "User message (excer")
         then A.(fg c_tool_name ++ st bold)
         else if String.length l > 0 && l.[0] = ' '
         then A.(fg c_fg_main)
         else A.(fg c_assistant) in
       push (attr, l))
   | None ->
     push (heading (Printf.sprintf "# link — turn %d (not indexed)"
                      effective_turn)));
  if show_link_edit then begin
    push (plain "");
    push (heading "─── this link's edit ───");
    (try
       let db = Urme_store.Schema.open_or_create
           ~project_dir:s.data.project_dir in
       let row = Urme_store.Edit_links.get db ~edit_key:link.edit_key in
       Urme_store.Schema.close db;
       match row with
       | None -> push (plain "  (edit row missing)")
       | Some r ->
         let split_push prefix attr text =
           String.split_on_char '\n' text
           |> List.iter (fun l -> push (attr, prefix ^ l)) in
         push (heading "--- old");
         split_push "- " A.(fg c_diff_del) r.old_content;
         push (plain "");
         push (heading "+++ new");
         split_push "+ " A.(fg c_diff_add) r.new_content
     with _ -> push (plain "  (error reading edit row)"))
  end;
  push (plain "");
  push (plain "(press Enter for the file diff at this commit)");
  List.rev !rows

let link_turn_styled s : (Notty.A.t * string) list Lwd.t =
  let link_lwd =
    Lwd.map2 (current_links s) (Lwd.get s.link_idx)
      ~f:(fun links idx ->
        let sorted = List.sort (fun (a : link) b ->
          let c = Int.compare a.turn_idx b.turn_idx in
          if c <> 0 then c else Int.compare a.entry_idx b.entry_idx) links in
        List.nth_opt sorted idx) in
  let view_and_offset =
    Lwd.map2 (Lwd.get s.hist_view) (Lwd.get s.link_turn_offset)
      ~f:(fun v o -> (v, o)) in
  Lwd.map2 link_lwd view_and_offset ~f:(fun link_opt (view, offset) ->
    match link_opt with
    | None -> []
    | Some link ->
      match view with
      | Overview -> render_link_overview ~offset link s
      | Body when link.session_id = "human" || link.session_id = "" ->
        render_link_overview ~offset link s
      | Body ->
        render_full_turn_styled
          ~highlight_edit_key:link.edit_key
          ~project_dir:s.data.project_dir
          ~session_id:link.session_id
          ~turn_idx:(link.turn_idx + offset) ())

let link_panel s =
  let focused = Lwd.map (Lwd.get s.focus) ~f:(fun f -> f = Links) in
  let label =
    Lwd.map (Lwd.get s.link_turn_offset) ~f:(fun off ->
      if off = 0 then "Link turn"
      else Printf.sprintf "Link turn  (offset %+d)" off) in
  Lwd.bind label ~f:(fun title ->
    styled_panel ~idx:5 ~focused ~title_text:title
      (link_turn_styled s) s.diff_scroll)

(* Reactive auto-scroll: when hist_view flips to Body AND the
   selection is on a real link, set diff_scroll to the line of the
   "»» THIS LINK'S EDIT »»" marker so the user lands right on the
   relevant tool_use. Fires once per (view, link) transition. *)
let link_body_scroll_driver s =
  let last_key = ref None in
  let link_lwd =
    Lwd.map2 (current_links s) (Lwd.get s.link_idx)
      ~f:(fun links idx ->
        let sorted = List.sort (fun (a : link) b ->
          let c = Int.compare a.turn_idx b.turn_idx in
          if c <> 0 then c else Int.compare a.entry_idx b.entry_idx) links in
        List.nth_opt sorted idx) in
  Lwd.map2
    (Lwd.map2 link_lwd (Lwd.get s.hist_view)
       ~f:(fun lk v -> (lk, v)))
    (Lwd.get s.link_turn_offset)
    ~f:(fun (link_opt, view) offset ->
      let key =
        Option.map (fun (l : link) -> l.edit_key) link_opt, view, offset in
      if !last_key <> Some key then begin
        last_key := Some key;
        match link_opt, view with
        | Some link, Body
          when link.session_id <> "human" && link.session_id <> "" ->
          let styled = render_full_turn_styled
            ~highlight_edit_key:link.edit_key
            ~project_dir:s.data.project_dir
            ~session_id:link.session_id
            ~turn_idx:(link.turn_idx + offset) () in
          let rec find i = function
            | [] -> 0
            | (_, line) :: rest ->
              if line = "  »» THIS LINK'S EDIT »»" then i
              else find (i + 1) rest in
          let scroll = max 0 (find 0 styled - 2) in
          Lwd.set s.diff_scroll scroll
        | _ -> ()
      end;
      U.empty)

let right_panel s =
  let inner = Lwd.bind (Lwd.get s.focus) ~f:(fun f ->
    if f = Links then link_panel s else diff_panel s) in
  with_mouse inner
    ~on_click:(fun () -> Lwd.set s.focus Links)
    ~on_scroll:(fun d ->
      Lwd.update (fun i -> max 0 (i + d)) s.diff_scroll)

(* -------- Search mode -------- *)

let short_sid sid =
  if String.length sid >= 8 then String.sub sid 0 8 else sid

let search_hit_summary (h : Urme_search.Search.hit) =
  let sid = match h.session_id with
    | Some s -> short_sid s | None -> "------" in
  let first_line s =
    match String.index_opt s '\n' with
    | Some i -> String.sub s 0 i
    | None -> s in
  let text =
    if h.summary <> "" then first_line h.summary
    else if h.prompt_text <> "" then first_line h.prompt_text
    else "(no text)" in
  Printf.sprintf "%s t%d  %s" sid h.turn_index text

(* Left panel — list of search hits, navigable by row click. *)
let search_results_panel s =
  let focused = Lwd.map (Lwd.get s.search_focus) ~f:(fun f -> f = Results) in
  let items_lwd = Lwd.map (Lwd.get s.search_hits)
    ~f:(List.map search_hit_summary) in
  let make =
    Lwd.map2
      (Lwd.map2 items_lwd (Lwd.get s.search_idx)
         ~f:(fun items sel ->
           dbg (Printf.sprintf "search_results_panel: recompute sel=%d items=%d"
                  sel (List.length items));
           (items, sel)))
      focused
      ~f:(fun (items, sel) focused ~w ~h ->
        render_panel ~idx:2 ~title:"Results"
          ~focused ~items ~sel_idx:sel ~w ~h)
  in
  let ui = with_size_sensor ~focused make in
  with_clickable_rows ui
    ~focus_set:(fun () -> Lwd.set s.search_focus Results)
    ~items_count:(fun () -> List.length (Lwd.peek s.search_hits))
    ~sel_var:s.search_idx
    ~on_scroll:(fun d ->
      let n = List.length (Lwd.peek s.search_hits) in
      let clamp i = max 0 (min (max 0 (n - 1)) i) in
      Lwd.set s.search_focus Results;
      Lwd.update (fun i -> clamp (i + d)) s.search_idx;
      Lwd.set s.search_scroll 0)

(* Styled lines for the selected hit's turn. Mirrors [link_turn_styled]:
   Overview = [render_turn_overview] (same rich summary shown in history
   and git-links), Body = [render_full_turn_styled]. *)
let search_body_styled s : (Notty.A.t * string) list Lwd.t =
  Lwd.map2
    (Lwd.map2 (Lwd.get s.search_hits) (Lwd.get s.search_idx)
       ~f:(fun hs idx -> List.nth_opt hs idx))
    (Lwd.map2 (Lwd.get s.search_view) (Lwd.get s.search_turn_offset)
       ~f:(fun v o -> (v, o)))
    ~f:(fun hit_opt (view, offset) ->
      match hit_opt with
      | None -> [(A.(fg c_fg_main), "(no results)")]
      | Some (h : Urme_search.Search.hit) ->
        let sid = Option.value h.session_id ~default:"" in
        let effective_turn = h.turn_index + offset in
        let overview_for_turn () =
          (* Prefer pre-indexed turn info → [render_turn_overview]
             (rich summary shared with history/links). Fall back to
             the hit's own summary if the turn isn't in the sessions
             cache (pre-index data drift). *)
          match List.find_opt (fun si -> si.session_id = sid)
                  s.data.sessions with
          | Some si ->
            (match List.find_opt (fun t -> t.t_turn_idx = effective_turn)
                     si.turns with
             | Some t ->
               let txt = render_turn_overview
                 ~project_dir:s.data.project_dir ~session_id:sid t in
               String.split_on_char '\n' txt
               |> List.map (fun l ->
                 let attr =
                   if String.length l > 0 && l.[0] = '#' then
                     A.(fg c_hunk ++ st bold)
                   else if String.length l >= 8 && String.sub l 0 8 = "Summary:"
                   then A.(fg c_assistant ++ st bold)
                   else if (String.length l >= 6 && String.sub l 0 6 = "Files ")
                        || (String.length l >= 9 && String.sub l 0 9 = "Commands ")
                        || (String.length l >= 8 && String.sub l 0 8 = "Actions ")
                        || (String.length l >= 19
                            && String.sub l 0 19 = "User message (excer")
                   then A.(fg c_tool_name ++ st bold)
                   else if String.length l > 0 && l.[0] = ' '
                   then A.(fg c_fg_main)
                   else A.(fg c_assistant) in
                 (attr, l))
             | None ->
               [(A.(fg c_hunk ++ st bold),
                 Printf.sprintf "# turn %d — %s" effective_turn sid);
                (A.(fg c_fg_main), "");
                (A.(fg c_fg_main), h.summary);
                (A.(fg c_fg_main), "");
                (A.(fg c_fg_main), h.prompt_text)])
          | None ->
            [(A.(fg c_hunk ++ st bold),
              Printf.sprintf "# turn %d — %s" effective_turn sid);
             (A.(fg c_fg_main), "");
             (A.(fg c_fg_main), h.summary);
             (A.(fg c_fg_main), "");
             (A.(fg c_fg_main), h.prompt_text)] in
        if sid = "" then
          [(A.(fg c_fg_main), h.summary);
           (A.(fg c_fg_main), "");
           (A.(fg c_fg_main), h.prompt_text)]
        else match view with
          | Overview -> overview_for_turn ()
          | Body ->
            render_full_turn_styled
              ~project_dir:s.data.project_dir
              ~session_id:sid
              ~turn_idx:effective_turn ())

let search_body_panel s =
  let focused = Lwd.map (Lwd.get s.search_focus) ~f:(fun f -> f = Results) in
  let label =
    Lwd.map2 (Lwd.get s.search_view) (Lwd.get s.search_turn_offset)
      ~f:(fun v off ->
        let v_str = match v with Overview -> "Overview" | Body -> "Body" in
        if off = 0 then v_str
        else Printf.sprintf "%s  (offset %+d)" v_str off) in
  let ui = Lwd.bind label ~f:(fun title ->
    styled_panel ~idx:3 ~focused ~title_text:title
      (search_body_styled s) s.search_scroll) in
  with_mouse ui
    ~on_click:(fun () -> ())
    ~on_scroll:(fun d ->
      Lwd.update (fun i -> max 0 (i + d)) s.search_scroll)

(* Query bar + info strip at top of search view. Uses nottui's
   [edit_field]. Submitting (Enter while Query-focused) fires off a
   background search; results come back via Lwt.async → Lwd.set. *)
(* No edit_field. The query is just text painted from [search_query];
   editing is done entirely through [on_key] when [search_focus = Query].
   This matches git-links: once the search has run, the tree is a pair
   of static panels (Results + Body) identical to the Links/Link-turn
   layout — no focus juggling, no embedded keyboard areas. *)
let search_query_bar s =
  (* The query text itself is already rendered in the top pane — this
     bar is just a one-line status/keybind hint. *)
  Lwd.map2 (Lwd.get s.search_focus) (Lwd.get s.search_running)
    ~f:(fun focus running ->
      let msg =
        if running then
          ("  ●  searching…  ", A.(fg c_hunk ++ st italic))
        else match focus with
          | Query ->
            ("  editing query — Enter: run   Esc: back   Backspace: delete  ",
             A.(fg c_hunk ++ st bold))
          | Results ->
            ("  results — ↑/↓ nav   Enter body/overview   ←/→ prev/next turn   /  edit query   s: new search  ",
             A.(fg (gray 14))) in
      let (text, attr) = msg in
      U.atom (I.string attr text))

let search_synthesis_panel s =
  let selected =
    Lwd.map2 (Lwd.get s.search_hits) (Lwd.get s.search_idx)
      ~f:(fun hs idx -> List.nth_opt hs idx) in
  let lines_lwd =
    Lwd.map2
      (Lwd.pair (Lwd.get s.search_query)
         (Lwd.pair (Lwd.get s.search_synthesis) (Lwd.get s.search_running)))
      selected
      ~f:(fun ((q, _), (syn, running)) selected ->
        let rows = ref [] in
        let push attr txt = rows := (attr, txt) :: !rows in
        if q <> "" then
          push A.(fg c_user ++ st bold) ("Query: " ^ q);
        if running then begin
          push A.(fg c_fg_main) "";
          push A.(fg c_hunk ++ st italic) "searching…"
        end else if syn <> "" then begin
          push A.(fg c_fg_main) "";
          push A.(fg c_assistant ++ st bold) "Synthesis:";
          String.split_on_char '\n' syn
          |> List.iter (fun l -> push A.(fg c_fg_main) ("  " ^ l))
        end;
        (match selected with
         | Some (h : Urme_search.Search.hit) ->
           push A.(fg c_fg_main) "";
           push A.(fg c_tool_name ++ st bold) "Selected summary:";
           let sum =
             if h.summary <> "" then h.summary
             else if h.prompt_text <> "" then h.prompt_text
             else "(no text)" in
           String.split_on_char '\n' sum
           |> List.iter (fun l -> push A.(fg c_fg_main) ("  " ^ l))
         | None ->
           if not running && syn = "" && q = "" then
             push A.(fg c_fg_main)
               "Type a query below, press Enter to search.");
        List.rev !rows) in
  let dummy = Lwd.var 0 in
  styled_panel ~idx:1 ~title_text:"Search"
    ~focused:(Lwd.pure false) lines_lwd dummy

let search_view s ~run_search:_ =
  let top = search_synthesis_panel s in
  let bottom = search_query_bar s in
  let left = search_results_panel s in
  let right = search_body_panel s in
  let split =
    Lwd.map2 left right ~f:(fun l r ->
      U.join_x
        (U.resize ~w:0 ~h:0 ~sw:1 ~sh:1 l)
        (U.resize ~w:0 ~h:0 ~sw:2 ~sh:1 r)) in
  (* Size the top panel dynamically: small by default, grows with
     the synthesis text (capped so results still get room). *)
  let count_lines s =
    if s = "" then 0 else List.length (String.split_on_char '\n' s) in
  let selected_summary =
    Lwd.map2 (Lwd.get s.search_hits) (Lwd.get s.search_idx)
      ~f:(fun hs idx ->
        match List.nth_opt hs idx with
        | None -> ""
        | Some (h : Urme_search.Search.hit) ->
          if h.summary <> "" then h.summary
          else if h.prompt_text <> "" then h.prompt_text
          else "") in
  let top_h =
    Lwd.map2 (Lwd.get s.search_synthesis) selected_summary
      ~f:(fun syn sum ->
        let syn_lines = count_lines syn in
        let sum_lines = count_lines sum in
        (* query (1) + blank + Synthesis: (1) + syn + blank +
           Selected summary: (1) + sum + borders (2 box + 1 blank) *)
        let n = 1 + 1 + syn_lines + 1 + 1 + sum_lines + 3 in
        min 20 (max 6 n)) in
  let top_sized = Lwd.map2 top top_h ~f:(fun ui h ->
    U.resize ~w:0 ~h ~sw:1 ~sh:0 ui) in
  Lwd.map2
    (Lwd.map2 top_sized split ~f:(fun a b -> (a, b)))
    bottom
    ~f:(fun (t, m) b ->
      U.join_y t
        (U.join_y
           (U.resize ~w:0 ~h:0 ~sw:1 ~sh:1 m)
           (U.resize ~w:0 ~h:1 ~sw:1 ~sh:0 b)))

(* Minimal reactivity diagnostic: a counter that redraws whenever a
   key is pressed. If this doesn't update either, the problem is in
   Nottui_lwt / the run loop, not in my panels. *)
let minimal_root s : U.t Lwd.t =
  let counter = Lwd.var 0 in
  let ui = Lwd.map (Lwd.get counter) ~f:(fun n ->
    dbg (Printf.sprintf "minimal: render n=%d" n);
    U.atom (I.string A.(fg lightcyan ++ st bold)
              (Printf.sprintf "COUNTER=%d  press + to bump, q to quit" n))) in
  let focus_handle = Nottui.Focus.make () in
  Nottui.Focus.request focus_handle;
  let focus_status = Nottui.Focus.status focus_handle in
  Lwd.map2 ui focus_status ~f:(fun inner focus ->
    U.keyboard_area ~focus (fun (key, _) ->
      match key with
      | `ASCII 'q' -> Lwt.wakeup_later s.quit_u (); `Handled
      | `ASCII '+' ->
        Lwd.update (fun n -> n + 1) counter;
        dbg (Printf.sprintf "minimal: +, now n=%d" (Lwd.peek counter));
        `Handled
      | _ -> `Unhandled) inner)

(* ---------- Top-level root ---------- *)

let root s : U.t Lwd.t =
  let _ = minimal_root in
  let config = Urme_core.Config.load () in
  let run_search query =
    dbg (Printf.sprintf "run_search: query=%S" query);
    Lwd.set s.search_running true;
    Lwd.set s.search_idx 0;
    Lwd.set s.search_scroll 0;
    Lwd.set s.search_view Overview;
    Lwd.set s.search_turn_offset 0;
    Lwd.set s.search_synthesis "";
    Lwt.async (fun () ->
      Lwt.catch
        (fun () ->
          let db = Urme_store.Schema.open_or_create
            ~project_dir:s.data.project_dir in
          let* (hits, synthesis) =
            Urme_search.Search.run_deep ~db
              ~binary:config.claude_binary
              ~limit:20
              ~project_dir:s.data.project_dir
              ~only_ranked:true
              query in
          dbg (Printf.sprintf "run_deep: %d hits, %d syn chars"
                 (List.length hits) (String.length synthesis));
          (try Urme_store.Schema.close db with _ -> ());
          Lwd.set s.search_hits hits;
          Lwd.set s.search_synthesis synthesis;
          Lwd.set s.search_running false;
          if hits <> [] then Lwd.set s.search_focus Results;
          Lwt.return_unit)
        (fun exn ->
          dbg (Printf.sprintf "run_deep EXN: %s" (Printexc.to_string exn));
          Lwd.set s.search_hits [];
          Lwd.set s.search_synthesis
            (Printf.sprintf "(error: %s)" (Printexc.to_string exn));
          Lwd.set s.search_running false;
          Lwt.return_unit)) in
  s.run_search <- run_search;
  let git_view =
    let left = W.vbox [
      branches_panel s;
      commits_panel s;
      files_panel s;
      links_panel s;
    ] in
    let right = right_panel s in
    (* Custom split — h_pane draws a loud yellow splitter we don't
   want. Just two stretched children joined horizontally. *)
Lwd.map2 left right ~f:(fun l r ->
  U.join_x
    (U.resize ~w:0 ~h:0 ~sw:1 ~sh:1 l)
    (U.resize ~w:0 ~h:0 ~sw:2 ~sh:1 r)) in
  let history_view =
    let left = W.vbox [ sessions_panel s; turns_panel s ] in
    Lwd.map2 left (history_body_panel s) ~f:(fun l r ->
      U.join_x
        (U.resize ~w:0 ~h:0 ~sw:1 ~sh:1 l)
        (U.resize ~w:0 ~h:0 ~sw:2 ~sh:1 r)) in
  let search_view_ui = search_view s ~run_search in
  let body = Lwd.bind (Lwd.get s.mode) ~f:(function
    | Git -> git_view
    | History -> history_view
    | Search -> search_view_ui) in
  let status =
    Lwd.map2
      (Lwd.pair (Lwd.get s.mode) (Lwd.get s.focus))
      (Lwd.pair (Lwd.get s.hist_view) (Lwd.get s.hist_focus))
      ~f:(fun (mode, focus) (view, _hfocus) ->
        let text = match mode, focus, view with
          | Git, Links, _ ->
            " [Git] Tab panel  ↑/↓ nav  ←/→ prev/next turn  Enter body/overview  Esc back  j/k scroll  h history  s search  q quit "
          | Git, _, _ ->
            " [Git] Tab panel  ↑/↓ nav  j/k diff scroll  J/K page  h history  s search  q quit "
          | History, _, Overview ->
            " [History] ↑/↓ nav  Tab panel  Enter body  g git  s search  q quit "
          | History, _, Body ->
            " [History] ↑/↓ prev/next turn  j/k scroll  J/K page  Enter back  g git  s search  q quit "
          | Search, _, _ ->
            " [Search] Enter run / body-overview  ↑/↓ nav  ←/→ prev/next turn  Tab focus  / or i edit query  Esc back  g git  h history  q quit "
        in
        U.resize ~sw:1 ~h:1 ~sh:0
          (U.atom (I.string A.(fg lightgreen) text))) in
  let drivers =
    Lwd.map (W.zbox
               [commits_driver s; content_driver s;
                link_body_scroll_driver s])
      ~f:(fun u -> U.resize ~w:0 ~h:0 ~sw:0 ~sh:0 u) in
  let stacked = W.vbox [body; status; drivers] in
  let focus_status = Nottui.Focus.status
    (let h = Nottui.Focus.make () in
     Nottui.Focus.request h; h) in
  Lwd.map2 stacked focus_status ~f:(fun ui focus ->
    dbg "root: building final UI";
    U.keyboard_area ~focus (on_key s) ui)

(* -------- Entry -------- *)

let run ~project_dir () =
  let project_dir =
    if project_dir = "." then Sys.getcwd ()
    else if Filename.is_relative project_dir
    then Filename.concat (Sys.getcwd ()) project_dir
    else project_dir in
  dbg "run: starting";
  let* data = Lwt.catch
    (fun () ->
      dbg "loading data…";
      let* d = load_data ~project_dir in
      dbg (Printf.sprintf "data loaded: %d branches, %d commits, %d links, %d sessions"
             (List.length d.branches) (List.length d.commits)
             (Hashtbl.length d.git_links) (List.length d.sessions));
      Lwt.return d)
    (fun e ->
      dbg (Printf.sprintf "load_data error: %s" (Printexc.to_string e));
      Lwt.fail e) in
  let quit_p, quit_u = Lwt.wait () in
  let s = {
    data;
    mode         = Lwd.var Git;
    focus        = Lwd.var Branches;
    branch_idx   = Lwd.var 0;
    commit_idx   = Lwd.var 0;
    file_idx     = Lwd.var 0;
    link_idx     = Lwd.var 0;
    diff_scroll  = Lwd.var 0;
    commits      = Lwd.var data.commits;
    current_diff = Lwd.var "";
    hist_focus   = Lwd.var Sessions;
    hist_view    = Lwd.var Overview;
    link_turn_offset = Lwd.var 0;
    session_idx  = Lwd.var 0;
    turn_idx     = Lwd.var 0;
    hist_scroll  = Lwd.var 0;
    search_focus   = Lwd.var Query;
    search_query   = Lwd.var ("", 0);
    search_hits    = Lwd.var [];
    search_idx     = Lwd.var 0;
    search_scroll  = Lwd.var 0;
    search_running = Lwd.var false;
    search_view    = Lwd.var Overview;
    search_turn_offset = Lwd.var 0;
    search_synthesis   = Lwd.var "";
    run_search         = (fun _ -> ());
    quit_u;
  } in
  (* Point branch_idx at the actually-checked-out branch; the
     [commits_driver] will then pick it up and load its commits,
     and [content_driver] will populate the right panel — all
     reactively, no seeding needed. *)
  (match List.mapi (fun i b -> (i, b)) data.branches
         |> List.find_opt (fun (_, b) -> b = data.current_branch) with
   | Some (i, _) -> Lwd.set s.branch_idx i
   | None -> ());
  (* MCP push bridge: the MCP server (separate process) writes tool
     results to a Unix socket; we receive them here and flip into the
     relevant mode with the state pre-populated — zero context bloat
     on Claude's side. *)
  let on_bridge_msg (json : Yojson.Safe.t) : unit Lwt.t =
    let open Yojson.Safe.Util in
    let msg_type =
      try json |> member "type" |> to_string with _ -> "" in
    dbg (Printf.sprintf "bridge: received %S" msg_type);
    (match msg_type with
     | "synthesis" ->
       let synthesis =
         try json |> member "synthesis" |> to_string with _ -> "" in
       let hits =
         try json |> member "cited" |> to_list
             |> List.filter_map (fun j ->
               try Some {
                 Urme_search.Search.step_id = 0;
                 session_id =
                   (match j |> member "session_id" with
                    | `String s -> Some s | _ -> None);
                 turn_index = j |> member "turn_index" |> to_int;
                 timestamp =
                   (try j |> member "timestamp" |> to_float with _ -> 0.);
                 summary =
                   (try j |> member "summary" |> to_string with _ -> "");
                 tags = "";
                 prompt_text =
                   (try j |> member "prompt_text" |> to_string with _ -> "");
                 files_touched = "[]";
                 commit_before = None;
                 commit_after = None;
                 score = 0.;
               } with _ -> None)
         with _ -> [] in
       Lwt.async (fun () ->
         let* () = Lwt.pause () in
         Lwd.set s.search_hits hits;
         Lwd.set s.search_synthesis synthesis;
         Lwd.set s.search_query ("", 0);
         Lwd.set s.search_idx 0;
         Lwd.set s.search_scroll 0;
         Lwd.set s.search_view Overview;
         Lwd.set s.search_turn_offset 0;
         Lwd.set s.search_running false;
         Lwd.set s.mode Search;
         (* Always land in Results so 'q' quits and '/' / 'i' returns
            to the query field. Previously focus went to Query on
            empty hits, swallowing 'q' into the search string. *)
         Lwd.set s.search_focus Results;
         Lwt.return_unit)
     | _ -> ());
    Lwt.return_unit in
  let* bridge =
    Lwt.catch
      (fun () ->
        let* br = Tui_bridge.start ~on_message:on_bridge_msg
          ~project_dir () in
        dbg "bridge: listening"; Lwt.return (Some br))
      (fun exn ->
        dbg (Printf.sprintf "bridge start failed: %s"
               (Printexc.to_string exn));
        Lwt.return None) in
  dbg "creating term";
  let term = Notty_lwt.Term.create ~mouse:true () in
  dbg "term created";
  let images =
    Nottui_lwt.render ~quit:quit_p
      ~size:(Notty_lwt.Term.size term)
      (Notty_lwt.Term.events term) (root s) in
  Lwt.finalize
    (fun () -> Lwt_stream.iter_s (Notty_lwt.Term.image term) images)
    (fun () ->
      let* () = Notty_lwt.Term.release term in
      match bridge with
      | None -> Lwt.return_unit
      | Some br ->
        Lwt.catch (fun () -> Tui_bridge.stop br)
          (fun _ -> Lwt.return_unit))

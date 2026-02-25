open Lwt.Syntax
open Types

type t = {
  base_sha : string;
  mutable steps : step list;
  mutable next_number : int;
  port : int;
  collections : collections;
}

let create ~base_sha ~port ~collections =
  { base_sha; steps = []; next_number = 1; port; collections }

let add t ~label ~intent ~conversation ~commit_sha ~diff ~files =
  let id = Uuidm.v4_gen (Random.State.make_self_init ()) () |> Uuidm.to_string in
  let s = {
    number = t.next_number;
    label;
    intent;
    conversation;
    commit_sha;
    diff;
    files;
    timestamp = Unix.gettimeofday ();
    experience_id = id;
  } in
  t.steps <- t.steps @ [s];
  t.next_number <- t.next_number + 1;
  (* Index in session collection for semantic search *)
  let* () = Chromadb.save_session_step ~port:t.port
    ~collections:t.collections s in
  Lwt.return s

let get t n =
  List.find_opt (fun s -> s.number = n) t.steps

let get_by_sha t sha =
  List.find_opt (fun s -> s.commit_sha = sha) t.steps

let all t = t.steps

let truncate_after t n =
  let to_remove = List.filter (fun s -> s.number > n) t.steps in
  t.steps <- List.filter (fun s -> s.number <= n) t.steps;
  (* Delete removed steps from all ChromaDB collections *)
  Lwt_list.iter_s (fun s ->
    Chromadb.delete ~port:t.port ~collections:t.collections
      ~id:s.experience_id
  ) to_remove

let format t =
  if t.steps = [] then "No steps recorded yet."
  else begin
    let lines = List.map (fun s ->
      Printf.sprintf "  %d. %s [%s] %s"
        s.number
        s.label
        (String.concat ", " s.files)
        (if s.conversation <> "" then "(saved)" else "")
    ) t.steps in
    String.concat "\n" ("Steps:" :: lines)
  end

let base_sha t = t.base_sha

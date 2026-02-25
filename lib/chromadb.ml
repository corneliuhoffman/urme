open Lwt.Syntax
open Types

let tenant = "default_tenant"
let database = "default_database"

let base_url port =
  Printf.sprintf "http://[::1]:%d/api/v2/tenants/%s/databases/%s"
    port tenant database

let ollama_url = "http://127.0.0.1:11434"

let safe_to_list j =
  match j with
  | `List l -> l
  | `Null -> []
  | _ -> []

let http_post_raw ~url ~body =
  let uri = Uri.of_string url in
  let headers = Cohttp.Header.of_list [
    "Content-Type", "application/json";
  ] in
  let body_str = Yojson.Safe.to_string body in
  let* _resp, body = Cohttp_lwt_unix.Client.post uri
    ~headers
    ~body:(Cohttp_lwt.Body.of_string body_str) in
  let+ body_str = Cohttp_lwt.Body.to_string body in
  Yojson.Safe.from_string body_str

let http_post ~port ~path ~body =
  http_post_raw ~url:(base_url port ^ path) ~body

let http_get ~port ~path =
  let uri = Uri.of_string (base_url port ^ path) in
  let* _resp, body = Cohttp_lwt_unix.Client.get uri in
  let+ body_str = Cohttp_lwt.Body.to_string body in
  Yojson.Safe.from_string body_str

(* Ollama embeddings *)
let embed_text text =
  let body = `Assoc [
    "model", `String "nomic-embed-text";
    "input", `String text;
  ] in
  let+ resp = http_post_raw ~url:(ollama_url ^ "/api/embed") ~body in
  let open Yojson.Safe.Util in
  resp |> member "embeddings" |> to_list |> List.hd |> to_list
  |> List.map to_float

let embed_texts texts =
  Lwt_list.map_s embed_text texts

(* Collections *)
let get_or_create_collection ~port ~name =
  let body = `Assoc [
    "name", `String name;
    "get_or_create", `Bool true;
  ] in
  let+ resp = http_post ~port ~path:"/collections" ~body in
  let open Yojson.Safe.Util in
  match resp |> member "error" |> to_string_option with
  | Some err ->
    let msg = resp |> member "message" |> to_string_option
      |> Option.value ~default:err in
    failwith (Printf.sprintf "ChromaDB error creating collection %s: %s" name msg)
  | None ->
    resp |> member "id" |> to_string

let ensure_collections ~port ~project =
  let* intents_id = get_or_create_collection ~port
    ~name:(project ^ "_intents") in
  let* conversations_id = get_or_create_collection ~port
    ~name:(project ^ "_conversations") in
  let+ session_id = get_or_create_collection ~port
    ~name:(project ^ "_session") in
  { intents_id; conversations_id; session_id }

(* Add document with embedding *)
let add_document ~port ~collection_id ~id ~document ~metadata =
  let* embedding = embed_text document in
  let body = `Assoc [
    "ids", `List [`String id];
    "documents", `List [`String document];
    "embeddings", `List [`List (List.map (fun f -> `Float f) embedding)];
    "metadatas", `List [metadata];
  ] in
  let+ _resp = http_post ~port
    ~path:(Printf.sprintf "/collections/%s/add" collection_id) ~body in
  ()

let save_experience ~port ~(collections : collections) (exp : experience) =
  let intent_doc = Printf.sprintf "%s: %s" exp.label exp.intent in
  let metadata = `Assoc [
    "diff", `String exp.diff;
    "files", `String (String.concat "," exp.files);
    "timestamp", `Float exp.timestamp;
    "label", `String exp.label;
    "intent", `String exp.intent;
    "conversation", `String exp.conversation;
    "commit_sha", `String exp.commit_sha;
  ] in
  let* () = add_document ~port ~collection_id:collections.intents_id
    ~id:exp.id ~document:intent_doc ~metadata in
  add_document ~port ~collection_id:collections.conversations_id
    ~id:exp.id ~document:exp.conversation ~metadata

let save_session_step ~port ~(collections : collections) (s : step) =
  let doc = Printf.sprintf "%s: %s" s.label s.intent in
  let metadata = `Assoc [
    "step_number", `Int s.number;
    "commit_sha", `String s.commit_sha;
    "diff", `String s.diff;
    "files", `String (String.concat "," s.files);
    "timestamp", `Float s.timestamp;
    "label", `String s.label;
    "intent", `String s.intent;
    "conversation", `String s.conversation;
  ] in
  add_document ~port ~collection_id:collections.session_id
    ~id:s.experience_id ~document:doc ~metadata

(* Query with embedding *)
let query_collection ~port ~collection_id ~query_text ~n =
  let* embedding = embed_text query_text in
  let body = `Assoc [
    "query_embeddings", `List [`List (List.map (fun f -> `Float f) embedding)];
    "n_results", `Int n;
    "include", `List [`String "documents"; `String "metadatas"; `String "distances"];
  ] in
  http_post ~port
    ~path:(Printf.sprintf "/collections/%s/query" collection_id) ~body

let parse_query_results (resp : Yojson.Safe.t) : (string * float * Yojson.Safe.t) list =
  let open Yojson.Safe.Util in
  let ids_outer = resp |> member "ids" |> safe_to_list in
  match ids_outer with
  | [] -> []
  | first_ids :: _ ->
    let ids = first_ids |> safe_to_list |> List.map to_string in
    let distances = (resp |> member "distances" |> safe_to_list
      |> function [] -> [] | d :: _ -> d |> safe_to_list |> List.map to_float) in
    let metadatas = (resp |> member "metadatas" |> safe_to_list
      |> function [] -> [] | m :: _ -> m |> safe_to_list) in
    if ids = [] then []
    else
      List.combine (List.combine ids distances) metadatas
      |> List.map (fun ((id, dist), meta) -> (id, dist, meta))

let experience_from_metadata ~id ~distance (meta : Yojson.Safe.t) : search_result =
  let open Yojson.Safe.Util in
  let exp = {
    id;
    intent = meta |> member "intent" |> to_string_option |> Option.value ~default:"";
    label = meta |> member "label" |> to_string_option |> Option.value ~default:"";
    conversation = meta |> member "conversation" |> to_string_option |> Option.value ~default:"";
    diff = meta |> member "diff" |> to_string_option |> Option.value ~default:"";
    files = (meta |> member "files" |> to_string_option |> Option.value ~default:""
             |> String.split_on_char ','
             |> List.filter (fun s -> String.length s > 0));
    timestamp = (try meta |> member "timestamp" |> to_float with _ -> 0.0);
    commit_sha = meta |> member "commit_sha" |> to_string_option |> Option.value ~default:"";
  } in
  { experience = exp; source = `Experience; distance }

let search_two_stage ~port ~(collections : collections) ~query ~broad_k ~n =
  (* Stage 1: broad search on intents *)
  let* intent_resp = query_collection ~port
    ~collection_id:collections.intents_id ~query_text:query ~n:broad_k in
  let intent_results = parse_query_results intent_resp in
  if intent_results = [] then Lwt.return []
  else begin
    (* Stage 2: re-rank by querying conversations for same IDs *)
    let* conv_resp = query_collection ~port
      ~collection_id:collections.conversations_id ~query_text:query
      ~n:broad_k in
    let conv_results = parse_query_results conv_resp in
    let conv_map = List.fold_left (fun acc (id, dist, _meta) ->
      (id, dist) :: acc
    ) [] conv_results in
    let combined = List.map (fun (id, intent_dist, meta) ->
      let conv_dist = match List.assoc_opt id conv_map with
        | Some d -> d
        | None -> 1.0
      in
      let combined_dist = (intent_dist +. conv_dist) /. 2.0 in
      (id, combined_dist, meta)
    ) intent_results in
    let sorted = List.sort (fun (_, d1, _) (_, d2, _) ->
      Float.compare d1 d2
    ) combined in
    let top_n = List.filteri (fun i _ -> i < n) sorted in
    Lwt.return (List.map (fun (id, dist, meta) ->
      (experience_from_metadata ~id ~distance:dist meta).experience
    ) top_n)
  end

let search_for_undo ~port ~(collections : collections) ~query ~n =
  let* session_resp = query_collection ~port
    ~collection_id:collections.session_id ~query_text:query ~n in
  let session_results = parse_query_results session_resp
    |> List.map (fun (id, dist, meta) ->
      { (experience_from_metadata ~id ~distance:dist meta) with
        source = `Session }) in
  let* intent_resp = query_collection ~port
    ~collection_id:collections.intents_id ~query_text:query ~n in
  let exp_results = parse_query_results intent_resp
    |> List.map (fun (id, dist, meta) ->
      experience_from_metadata ~id ~distance:dist meta) in
  let all = session_results @ exp_results in
  let sorted = List.sort (fun a b ->
    Float.compare a.distance b.distance
  ) all in
  Lwt.return (List.filteri (fun i _ -> i < n) sorted)

let delete ~port ~(collections : collections) ~id =
  let delete_from coll_id =
    let body = `Assoc [
      "ids", `List [`String id];
    ] in
    let+ _resp = http_post ~port
      ~path:(Printf.sprintf "/collections/%s/delete" coll_id) ~body in
    ()
  in
  let* () = delete_from collections.intents_id in
  let* () = delete_from collections.conversations_id in
  delete_from collections.session_id

let list_all ~port ~(collections : collections) ~limit =
  let body = `Assoc [
    "limit", `Int limit;
    "include", `List [`String "documents"; `String "metadatas"];
  ] in
  let+ resp = http_post ~port
    ~path:(Printf.sprintf "/collections/%s/get" collections.intents_id) ~body in
  let open Yojson.Safe.Util in
  let ids = resp |> member "ids" |> safe_to_list |> List.map to_string in
  let metadatas = resp |> member "metadatas" |> safe_to_list in
  if ids = [] then []
  else
    List.combine ids metadatas
    |> List.map (fun (id, meta) ->
      (experience_from_metadata ~id ~distance:0.0 meta).experience)

let clear_session ~port ~(collections : collections) =
  let body = `Assoc [
    "include", `List [`String "documents"];
  ] in
  let* resp = http_post ~port
    ~path:(Printf.sprintf "/collections/%s/get" collections.session_id) ~body in
  let open Yojson.Safe.Util in
  let ids = resp |> member "ids" |> safe_to_list |> List.map to_string in
  if ids = [] then Lwt.return_unit
  else begin
    let del_body = `Assoc [
      "ids", `List (List.map (fun id -> `String id) ids);
    ] in
    let+ _resp = http_post ~port
      ~path:(Printf.sprintf "/collections/%s/delete" collections.session_id)
      ~body:del_body in
    ()
  end

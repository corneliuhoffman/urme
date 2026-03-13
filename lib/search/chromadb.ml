open Lwt.Syntax
open Urme_core.Types

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

let http_delete ~port ~path =
  let uri = Uri.of_string (base_url port ^ path) in
  let* _resp, body = Cohttp_lwt_unix.Client.delete uri in
  let+ body_str = Cohttp_lwt.Body.to_string body in
  ignore body_str

(* Ollama chat — summarize conversation *)
let summarize_text text =
  let prompt = Printf.sprintf
    "Summarize this coding conversation in 2-3 sentences. Focus on: what was the goal, what approach was taken, and what was the outcome. Be specific about technologies, files, and patterns.\n\n%s"
    text in
  let body = `Assoc [
    "model", `String "llama3.2";
    "prompt", `String prompt;
    "stream", `Bool false;
  ] in
  let+ resp = http_post_raw ~url:(ollama_url ^ "/api/generate") ~body in
  let open Yojson.Safe.Util in
  resp |> member "response" |> to_string_option |> Option.value ~default:text

(* Split text into chunks of at most n chars, breaking at newlines *)
let chunk_text ~max_chars text =
  let len = String.length text in
  if len <= max_chars then [text]
  else
    let chunks = ref [] in
    let pos = ref 0 in
    while !pos < len do
      let remaining = len - !pos in
      let chunk_end = if remaining <= max_chars then len
        else
          (* Look for last newline within range *)
          let limit = !pos + max_chars in
          let nl = ref limit in
          while !nl > !pos && String.get text !nl <> '\n' do decr nl done;
          if !nl > !pos then !nl + 1 else limit
      in
      chunks := String.sub text !pos (chunk_end - !pos) :: !chunks;
      pos := chunk_end
    done;
    List.rev !chunks

(* Embed a single chunk via Ollama *)
let embed_single chunk =
  let body = `Assoc [
    "model", `String "nomic-embed-text";
    "input", `String chunk;
  ] in
  let+ resp = http_post_raw ~url:(ollama_url ^ "/api/embed") ~body in
  let open Yojson.Safe.Util in
  match resp |> member "error" |> to_string_option with
  | Some err -> failwith (Printf.sprintf "Ollama embed error: %s" err)
  | None ->
    match resp |> member "embeddings" with
    | `Null | `Bool _ | `String _ ->
      failwith (Printf.sprintf "Ollama returned no embeddings (response: %s)"
        (Yojson.Safe.to_string resp |> fun s ->
          if String.length s > 200 then String.sub s 0 200 ^ "..." else s))
    | embeddings ->
      embeddings |> to_list |> List.hd |> to_list |> List.map to_float

(* Average a list of embedding vectors *)
let average_embeddings vecs =
  match vecs with
  | [] -> []
  | [v] -> v
  | first :: _ ->
    let n = Float.of_int (List.length vecs) in
    let sums = Array.make (List.length first) 0.0 in
    List.iter (fun v ->
      List.iteri (fun i x -> sums.(i) <- sums.(i) +. x) v
    ) vecs;
    Array.to_list (Array.map (fun s -> s /. n) sums)

(* Ollama embeddings — splits long text into chunks and averages *)
let embed_text text =
  let chunks = chunk_text ~max_chars:2000 text in
  let* embeddings = Lwt_list.map_s embed_single chunks in
  Lwt.return (average_embeddings embeddings)

(* Summarize then embed *)
let summarize_and_embed text =
  let* summary = summarize_text text in
  let+ embedding = embed_text summary in
  (summary, embedding)

(* Single collection per project *)
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

let delete_collection ~port ~name =
  let* _id = get_or_create_collection ~port ~name in
  http_delete ~port ~path:(Printf.sprintf "/collections/%s" name)

let ensure_collection ~port ~project =
  get_or_create_collection ~port ~name:(project ^ "_experiences")

let ensure_interactions_collection ~port ~project =
  get_or_create_collection ~port ~name:(project ^ "_interactions")

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

(* Save an experience — summarize full conversation via Ollama, then embed the summary.
   Experience ID = commit SHA. Stores git metadata alongside conversation data. *)
let save_experience ~port ~collection_id (exp : experience) ~full_conversation =
  let* (summary, embedding) = summarize_and_embed full_conversation in
  let metadata = `Assoc [
    "label", `String exp.label;
    "intent", `String exp.intent;
    "session_id", `String exp.session_id;
    "interaction_uuids", `String (String.concat "," exp.interaction_uuids);
    "head_sha", `String exp.head_sha;
    "commit_sha", `String exp.commit_sha;
    "commit_message", `String exp.commit_message;
    "parent_sha", `String exp.parent_sha;
    "diff", `String (if String.length exp.diff <= 10000 then exp.diff
                      else String.sub exp.diff 0 10000 ^ "\n... (truncated)");
    "branch", `String exp.branch;
    "files_changed", `String (String.concat "," exp.files_changed);
    "timestamp", `Float exp.timestamp;
    "reverted", `Bool exp.reverted;
    "summary", `String summary;
    "conversation_text", `String full_conversation;
  ] in
  let body = `Assoc [
    "ids", `List [`String exp.id];
    "documents", `List [`String summary];
    "embeddings", `List [`List (List.map (fun f -> `Float f) embedding)];
    "metadatas", `List [metadata];
  ] in
  let+ _resp = http_post ~port
    ~path:(Printf.sprintf "/collections/%s/add" collection_id) ~body in
  ()

(* Search experiences *)
let search ~port ~collection_id ~query ~n =
  let* embedding = embed_text query in
  let body = `Assoc [
    "query_embeddings", `List [`List (List.map (fun f -> `Float f) embedding)];
    "n_results", `Int n;
    "include", `List [`String "documents"; `String "metadatas"; `String "distances"];
  ] in
  let+ resp = http_post ~port
    ~path:(Printf.sprintf "/collections/%s/query" collection_id) ~body in
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
      let results = List.combine (List.combine ids distances) metadatas
        |> List.map (fun ((id, dist), meta) ->
          let exp = experience_of_metadata id meta in
          (* Deprioritize reverted experiences *)
          let adjusted_dist = if exp.reverted then dist +. 0.5 else dist in
          { experience = exp; distance = adjusted_dist })
      in
      List.sort (fun a b -> Float.compare a.distance b.distance) results

(* Update metadata for an experience (for link_commit and go_back).
   Returns false if the document doesn't exist. *)
let update_metadata ~port ~collection_id ~id ~updates =
  let get_body = `Assoc [
    "ids", `List [`String id];
    "include", `List [`String "metadatas"];
  ] in
  let* resp = http_post ~port
    ~path:(Printf.sprintf "/collections/%s/get" collection_id) ~body:get_body in
  let open Yojson.Safe.Util in
  let ids = resp |> member "ids" |> safe_to_list |> List.map to_string in
  match ids with
  | [] -> Lwt.return false
  | _ ->
    let old_meta = resp |> member "metadatas" |> safe_to_list
      |> List.hd in
    let old_fields = match old_meta with `Assoc l -> l | _ -> [] in
    let new_fields = List.map (fun (k, v) ->
      match List.assoc_opt k updates with
      | Some new_v -> (k, new_v)
      | None -> (k, v)
    ) old_fields in
    let extra = List.filter (fun (k, _) ->
      not (List.mem_assoc k old_fields)
    ) updates in
    let merged = `Assoc (new_fields @ extra) in
    let update_body = `Assoc [
      "ids", `List [`String id];
      "metadatas", `List [merged];
    ] in
    let+ _resp = http_post ~port
      ~path:(Printf.sprintf "/collections/%s/update" collection_id)
      ~body:update_body in
    true

(* List all experiences *)
let list_all ~port ~collection_id ~limit =
  let body = `Assoc [
    "limit", `Int limit;
    "include", `List [`String "documents"; `String "metadatas"];
  ] in
  let+ resp = http_post ~port
    ~path:(Printf.sprintf "/collections/%s/get" collection_id) ~body in
  let open Yojson.Safe.Util in
  let ids = resp |> member "ids" |> safe_to_list |> List.map to_string in
  let metadatas = resp |> member "metadatas" |> safe_to_list in
  if ids = [] then []
  else
    List.combine ids metadatas
    |> List.map (fun (id, meta) -> experience_of_metadata id meta)

(* List all experiences with their Ollama-generated summaries *)
let list_all_with_summaries ~port ~collection_id ~limit =
  let body = `Assoc [
    "limit", `Int limit;
    "include", `List [`String "documents"; `String "metadatas"];
  ] in
  let+ resp = http_post ~port
    ~path:(Printf.sprintf "/collections/%s/get" collection_id) ~body in
  let open Yojson.Safe.Util in
  let ids = resp |> member "ids" |> safe_to_list |> List.map to_string in
  let metadatas = resp |> member "metadatas" |> safe_to_list in
  if ids = [] then []
  else
    List.combine ids metadatas
    |> List.map (fun (id, meta) ->
      let exp = experience_of_metadata id meta in
      let summary = meta |> member "summary" |> to_string_option
        |> Option.value ~default:"" in
      (exp, summary))

(* Delete an experience *)
let delete ~port ~collection_id ~id =
  let body = `Assoc [
    "ids", `List [`String id];
  ] in
  let+ _resp = http_post ~port
    ~path:(Printf.sprintf "/collections/%s/delete" collection_id) ~body in
  ()

(* Get metadata for an interaction by ID. Returns None if not found. *)
let get_interaction_meta ~port ~collection_id ~id =
  let body = `Assoc [
    "ids", `List [`String id];
    "include", `List [`String "metadatas"];
  ] in
  let* resp = http_post ~port
    ~path:(Printf.sprintf "/collections/%s/get" collection_id) ~body in
  let open Yojson.Safe.Util in
  let ids = resp |> member "ids" |> safe_to_list |> List.map to_string in
  match ids with
  | [] -> Lwt.return_none
  | _ ->
    let meta = resp |> member "metadatas" |> safe_to_list |> List.hd in
    Lwt.return_some meta

(* Save a single interaction document into the interactions collection.
   The document text is "User: <question>\nAssistant: <summary truncated to 500 chars>" *)
let save_interaction ~port ~collection_id ~experience_id ~interaction_index
    ~user_text ~assistant_summary ~user_uuid ~timestamp
    ?(git_info="") () =
  let summary_trunc =
    if String.length assistant_summary <= 500 then assistant_summary
    else String.sub assistant_summary 0 500 ^ "..." in
  let document = Printf.sprintf "User: %s\nAssistant: %s" user_text summary_trunc in
  let* embedding = embed_text document in
  let id = Printf.sprintf "%s_%d" experience_id interaction_index in
  let git_fields = if git_info <> "" then [
    "git_info", `String git_info;
  ] else [] in
  let metadata = `Assoc ([
    "experience_id", `String experience_id;
    "interaction_index", `Int interaction_index;
    "user_uuid", `String user_uuid;
    "user_text", `String (if String.length user_text <= 200 then user_text
                          else String.sub user_text 0 200 ^ "...");
    "timestamp", `String timestamp;
  ] @ git_fields) in
  let body = `Assoc [
    "ids", `List [`String id];
    "documents", `List [`String document];
    "embeddings", `List [`List (List.map (fun f -> `Float f) embedding)];
    "metadatas", `List [metadata];
  ] in
  let+ _resp = http_post ~port
    ~path:(Printf.sprintf "/collections/%s/add" collection_id) ~body in
  ()

(* Update git_info JSON on an existing interaction *)
let update_interaction_git_info ~port ~collection_id ~id ~git_info =
  let updates = [
    "git_info", `String git_info;
  ] in
  update_metadata ~port ~collection_id ~id ~updates

(* Search interactions within a specific experience *)
let search_interactions ~port ~collection_id ~experience_id ~query ~n =
  let* embedding = embed_text query in
  let body = `Assoc [
    "query_embeddings", `List [`List (List.map (fun f -> `Float f) embedding)];
    "n_results", `Int n;
    "where", `Assoc ["experience_id", `Assoc ["$eq", `String experience_id]];
    "include", `List [`String "documents"; `String "metadatas"; `String "distances"];
  ] in
  let+ resp = http_post ~port
    ~path:(Printf.sprintf "/collections/%s/query" collection_id) ~body in
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
      |> List.map (fun ((_id, distance), meta) ->
        let idx = meta |> member "interaction_index" |> to_int_option
          |> Option.value ~default:0 in
        let user_uuid = meta |> member "user_uuid" |> to_string_option
          |> Option.value ~default:"" in
        (idx, user_uuid, distance))

(* Search all interactions across all sessions *)
let search_all_interactions ~port ~collection_id ~query ~n =
  let* embedding = embed_text query in
  let body = `Assoc [
    "query_embeddings", `List [`List (List.map (fun f -> `Float f) embedding)];
    "n_results", `Int n;
    "include", `List [`String "documents"; `String "metadatas"; `String "distances"];
  ] in
  let+ resp = http_post ~port
    ~path:(Printf.sprintf "/collections/%s/query" collection_id) ~body in
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
    let documents = (resp |> member "documents" |> safe_to_list
      |> function [] -> [] | d :: _ -> d |> safe_to_list
         |> List.map (fun j -> to_string_option j |> Option.value ~default:"")) in
    if ids = [] then []
    else
      let combine4 a b c d =
        List.map2 (fun (a,b) (c,d) -> (a,b,c,d))
          (List.combine a b) (List.combine c d) in
      combine4 ids distances metadatas documents
      |> List.map (fun (_id, distance, meta, doc) ->
        let session_id = meta |> member "experience_id" |> to_string_option
          |> Option.value ~default:"" in
        let user_text = meta |> member "user_text" |> to_string_option
          |> Option.value ~default:"" in
        let timestamp = meta |> member "timestamp" |> to_string_option
          |> Option.value ~default:"" in
        let interaction_index = meta |> member "interaction_index" |> to_int_option
          |> Option.value ~default:0 in
        (session_id, user_text, doc, interaction_index, timestamp, distance))

(* Fetch all interactions that have non-empty git_info, paged.
   Returns list of (id, git_info_string, experience_id, interaction_index). *)
let get_all_with_git_info ~port ~collection_id =
  let rec fetch_page ~offset ~acc =
    let body = `Assoc [
      "include", `List [`String "metadatas"];
      "limit", `Int 500;
      "offset", `Int offset;
    ] in
    let* resp = http_post ~port
      ~path:(Printf.sprintf "/collections/%s/get" collection_id) ~body in
    let open Yojson.Safe.Util in
    let ids = resp |> member "ids" |> safe_to_list |> List.map to_string in
    let metadatas = resp |> member "metadatas" |> safe_to_list in
    let found = List.filter_map (fun (id, meta) ->
      let gi = meta |> member "git_info" |> to_string_option
        |> Option.value ~default:"" in
      if gi <> "" then
        let session_id = meta |> member "experience_id" |> to_string_option
          |> Option.value ~default:"" in
        let idx = meta |> member "interaction_index" |> to_int_option
          |> Option.value ~default:0 in
        let ts = meta |> member "timestamp" |> to_string_option
          |> Option.value ~default:"" in
        Some (id, gi, session_id, idx, ts)
      else None
    ) (List.combine ids metadatas) in
    let acc = acc @ found in
    if List.length ids < 500 then
      Lwt.return acc
    else
      fetch_page ~offset:(offset + 500) ~acc
  in
  fetch_page ~offset:0 ~acc:[]

(* Fetch all interaction IDs in the collection (paged). *)
let get_all_interaction_ids ~port ~collection_id =
  let rec fetch_page ~offset ~acc =
    let body = `Assoc [
      "include", `List [];
      "limit", `Int 5000;
      "offset", `Int offset;
    ] in
    let* resp = http_post ~port
      ~path:(Printf.sprintf "/collections/%s/get" collection_id) ~body in
    let open Yojson.Safe.Util in
    let ids = resp |> member "ids" |> safe_to_list |> List.map to_string in
    let acc = acc @ ids in
    if List.length ids < 5000 then Lwt.return acc
    else fetch_page ~offset:(offset + 5000) ~acc
  in
  fetch_page ~offset:0 ~acc:[]

(* Delete all interaction documents for an experience *)
let delete_interactions ~port ~collection_id ~experience_id =
  let body = `Assoc [
    "where", `Assoc ["experience_id", `Assoc ["$eq", `String experience_id]];
  ] in
  let+ _resp = http_post ~port
    ~path:(Printf.sprintf "/collections/%s/delete" collection_id) ~body in
  ()

(* An interaction: user message + all responses until next user message *)
(* Main JSONL is linear — simple line-range segmentation *)
type interaction = {
  index : int;
  line_start : int;
  line_end : int;
  user_uuid : string;
  timestamp : string;
  user_text : string;
  assistant_summary : string;
  files_changed : string list;
  branch : string;
}

(* An experience is a commit.
   The commit SHA is the canonical experience ID.
   Experience metadata is encoded in the commit message. *)
type experience = {
  id : string;                    (* = commit SHA *)
  label : string;
  intent : string;
  session_id : string;
  interaction_uuids : string list;
  head_sha : string;             (* HEAD at save time *)
  commit_sha : string;           (* linked commit SHA (may differ from id for retroactive links) *)
  commit_message : string;       (* full commit message *)
  parent_sha : string;           (* parent commit SHA *)
  diff : string;                 (* unified diff *)
  branch : string;
  files_changed : string list;
  timestamp : float;
  reverted : bool;
}

type search_result = {
  experience : experience;
  distance : float;
}

(* Serialization *)

let interaction_to_yojson (i : interaction) : Yojson.Safe.t =
  `Assoc [
    "index", `Int i.index;
    "line_start", `Int i.line_start;
    "line_end", `Int i.line_end;
    "user_uuid", `String i.user_uuid;
    "timestamp", `String i.timestamp;
    "user_text", `String i.user_text;
    "assistant_summary", `String i.assistant_summary;
    "files_changed", `List (List.map (fun f -> `String f) i.files_changed);
    "branch", `String i.branch;
  ]

let experience_to_yojson (e : experience) : Yojson.Safe.t =
  `Assoc [
    "id", `String e.id;
    "label", `String e.label;
    "intent", `String e.intent;
    "session_id", `String e.session_id;
    "interaction_uuids", `List (List.map (fun u -> `String u) e.interaction_uuids);
    "head_sha", `String e.head_sha;
    "commit_sha", `String e.commit_sha;
    "commit_message", `String e.commit_message;
    "parent_sha", `String e.parent_sha;
    "diff", `String e.diff;
    "branch", `String e.branch;
    "files_changed", `List (List.map (fun f -> `String f) e.files_changed);
    "timestamp", `Float e.timestamp;
    "reverted", `Bool e.reverted;
  ]

let experience_of_metadata (id : string) (meta : Yojson.Safe.t) : experience =
  let open Yojson.Safe.Util in
  let str key = meta |> member key |> to_string_option |> Option.value ~default:"" in
  let split_csv s =
    String.split_on_char ',' s |> List.filter (fun s -> String.length s > 0) in
  {
    id;
    label = str "label";
    intent = str "intent";
    session_id = str "session_id";
    interaction_uuids = split_csv (str "interaction_uuids");
    head_sha = str "head_sha";
    commit_sha = str "commit_sha";
    commit_message = str "commit_message";
    parent_sha = str "parent_sha";
    diff = str "diff";
    branch = str "branch";
    files_changed = split_csv (str "files_changed");
    timestamp = (try meta |> member "timestamp" |> to_float with _ -> 0.0);
    reverted = (try meta |> member "reverted" |> to_bool with _ -> false);
  }

let search_result_to_yojson (r : search_result) : Yojson.Safe.t =
  `Assoc [
    "experience", experience_to_yojson r.experience;
    "distance", `Float r.distance;
  ]

(* --- New types for urme session management --- *)

type role = User | Assistant | System

type content_block =
  | Text of string
  | Thinking of string
  | Tool_use of { name : string; id : string; input : Yojson.Safe.t }
  | Tool_result of { tool_use_id : string; content : string; is_error : bool }

type message = {
  role : role;
  content : content_block list;
  timestamp : float;
}

type artifact =
  | PR of { number : int; url : string; title : string }
  | Issue of { number : int; url : string }
  | Review of { pr_number : int; review_id : int }

type session = {
  id : string;
  started_at : float;
  repo_path : string;
  branch : string;
  base_commit : string;
  produced_commits : string list;
  touched_files : string list;
  conversation : message list;
  github_artifacts : artifact list;
}

type repo_id = {
  path : string;
  url_hash : string;
}

(* Serialization for new types *)

let role_to_string = function
  | User -> "user" | Assistant -> "assistant" | System -> "system"

let role_of_string = function
  | "user" -> User | "assistant" -> Assistant | "system" -> System
  | _ -> User

let content_block_to_yojson = function
  | Text s -> `Assoc ["type", `String "text"; "text", `String s]
  | Thinking s -> `Assoc ["type", `String "thinking"; "thinking", `String s]
  | Tool_use { name; id; input } ->
    `Assoc ["type", `String "tool_use"; "name", `String name;
            "id", `String id; "input", input]
  | Tool_result { tool_use_id; content; is_error } ->
    `Assoc ["type", `String "tool_result"; "tool_use_id", `String tool_use_id;
            "content", `String content; "is_error", `Bool is_error]

let content_block_of_yojson json =
  let open Yojson.Safe.Util in
  match json |> member "type" |> to_string with
  | "text" -> Text (json |> member "text" |> to_string)
  | "thinking" -> Thinking (json |> member "thinking" |> to_string)
  | "tool_use" ->
    Tool_use {
      name = json |> member "name" |> to_string;
      id = json |> member "id" |> to_string;
      input = json |> member "input";
    }
  | "tool_result" ->
    Tool_result {
      tool_use_id = json |> member "tool_use_id" |> to_string;
      content = json |> member "content" |> to_string;
      is_error = (try json |> member "is_error" |> to_bool with _ -> false);
    }
  | _ -> Text ""

let message_to_yojson (m : message) : Yojson.Safe.t =
  `Assoc [
    "role", `String (role_to_string m.role);
    "content", `List (List.map content_block_to_yojson m.content);
    "timestamp", `Float m.timestamp;
  ]

let message_of_yojson json : message =
  let open Yojson.Safe.Util in
  {
    role = role_of_string (json |> member "role" |> to_string);
    content = json |> member "content" |> to_list
              |> List.map content_block_of_yojson;
    timestamp = (try json |> member "timestamp" |> to_float with _ -> 0.0);
  }

let artifact_to_yojson = function
  | PR { number; url; title } ->
    `Assoc ["type", `String "pr"; "number", `Int number;
            "url", `String url; "title", `String title]
  | Issue { number; url } ->
    `Assoc ["type", `String "issue"; "number", `Int number;
            "url", `String url]
  | Review { pr_number; review_id } ->
    `Assoc ["type", `String "review"; "pr_number", `Int pr_number;
            "review_id", `Int review_id]

let artifact_of_yojson json =
  let open Yojson.Safe.Util in
  match json |> member "type" |> to_string with
  | "pr" -> PR {
      number = json |> member "number" |> to_int;
      url = json |> member "url" |> to_string;
      title = json |> member "title" |> to_string;
    }
  | "issue" -> Issue {
      number = json |> member "number" |> to_int;
      url = json |> member "url" |> to_string;
    }
  | "review" -> Review {
      pr_number = json |> member "pr_number" |> to_int;
      review_id = json |> member "review_id" |> to_int;
    }
  | _ -> Issue { number = 0; url = "" }

let session_to_yojson (s : session) : Yojson.Safe.t =
  `Assoc [
    "id", `String s.id;
    "started_at", `Float s.started_at;
    "repo_path", `String s.repo_path;
    "branch", `String s.branch;
    "base_commit", `String s.base_commit;
    "produced_commits", `List (List.map (fun c -> `String c) s.produced_commits);
    "touched_files", `List (List.map (fun f -> `String f) s.touched_files);
    "conversation", `List (List.map message_to_yojson s.conversation);
    "github_artifacts", `List (List.map artifact_to_yojson s.github_artifacts);
  ]

let session_of_yojson json : session =
  let open Yojson.Safe.Util in
  {
    id = json |> member "id" |> to_string;
    started_at = json |> member "started_at" |> to_float;
    repo_path = json |> member "repo_path" |> to_string;
    branch = json |> member "branch" |> to_string;
    base_commit = json |> member "base_commit" |> to_string;
    produced_commits = json |> member "produced_commits" |> to_list
                       |> List.map to_string;
    touched_files = json |> member "touched_files" |> to_list
                    |> List.map to_string;
    conversation = json |> member "conversation" |> to_list
                   |> List.map message_of_yojson;
    github_artifacts = json |> member "github_artifacts" |> to_list
                       |> List.map artifact_of_yojson;
  }

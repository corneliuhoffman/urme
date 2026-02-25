type step = {
  number : int;
  label : string;
  intent : string;
  conversation : string;
  commit_sha : string;
  diff : string;
  files : string list;
  timestamp : float;
  experience_id : string;
}

type experience = {
  id : string;
  intent : string;
  label : string;
  conversation : string;
  diff : string;
  files : string list;
  timestamp : float;
  commit_sha : string;
}

type search_result = {
  experience : experience;
  source : [ `Session | `Experience ];
  distance : float;
}

type collections = {
  intents_id : string;
  conversations_id : string;
  session_id : string;
}

let step_to_experience (s : step) : experience =
  { id = s.experience_id;
    intent = s.intent;
    label = s.label;
    conversation = s.conversation;
    diff = s.diff;
    files = s.files;
    timestamp = s.timestamp;
    commit_sha = s.commit_sha;
  }

let experience_to_yojson (e : experience) : Yojson.Safe.t =
  `Assoc [
    "id", `String e.id;
    "intent", `String e.intent;
    "label", `String e.label;
    "conversation", `String e.conversation;
    "diff", `String e.diff;
    "files", `List (List.map (fun f -> `String f) e.files);
    "timestamp", `Float e.timestamp;
    "commit_sha", `String e.commit_sha;
  ]

let experience_of_yojson (j : Yojson.Safe.t) : experience =
  let open Yojson.Safe.Util in
  { id = j |> member "id" |> to_string;
    intent = j |> member "intent" |> to_string;
    label = j |> member "label" |> to_string;
    conversation = j |> member "conversation" |> to_string_option |> Option.value ~default:"";
    diff = j |> member "diff" |> to_string_option |> Option.value ~default:"";
    files = j |> member "files" |> to_list |> List.map to_string;
    timestamp = j |> member "timestamp" |> to_float;
    commit_sha = j |> member "commit_sha" |> to_string_option |> Option.value ~default:"";
  }

let step_to_yojson (s : step) : Yojson.Safe.t =
  `Assoc [
    "number", `Int s.number;
    "label", `String s.label;
    "intent", `String s.intent;
    "commit_sha", `String s.commit_sha;
    "files", `List (List.map (fun f -> `String f) s.files);
    "timestamp", `Float s.timestamp;
    "experience_id", `String s.experience_id;
  ]

let search_result_to_yojson (r : search_result) : Yojson.Safe.t =
  `Assoc [
    "experience", experience_to_yojson r.experience;
    "source", `String (match r.source with `Session -> "session" | `Experience -> "experience");
    "distance", `Float r.distance;
  ]

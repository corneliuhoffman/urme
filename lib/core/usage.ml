open Lwt.Syntax

(* Fetch Claude Pro/Max subscription usage via the OAuth usage API.

   Endpoint: GET https://api.anthropic.com/api/oauth/usage
   Auth: Bearer token from macOS Keychain ("Claude Code-credentials")
   Returns utilization percentages (0-100) for 5-hour and 7-day windows. *)

type window = {
  utilization : float;
  resets_at : string;
}

type usage_data = {
  five_hour : window option;
  seven_day : window option;
  seven_day_sonnet : window option;
  seven_day_opus : window option;
}

let empty = {
  five_hour = None; seven_day = None;
  seven_day_sonnet = None; seven_day_opus = None;
}

(* Read OAuth access token from macOS Keychain *)
let read_access_token () =
  Lwt.catch (fun () ->
    let cmd = Lwt_process.shell
      "security find-generic-password -s 'Claude Code-credentials' -w 2>/dev/null" in
    let* raw = Lwt_process.pread cmd in
    let raw = String.trim raw in
    if raw = "" then Lwt.return_none
    else
      try
        let json = Yojson.Safe.from_string raw in
        let open Yojson.Safe.Util in
        let token = json |> member "claudeAiOauth" |> member "accessToken"
                    |> to_string in
        Lwt.return_some token
      with _ -> Lwt.return_none
  ) (fun _exn -> Lwt.return_none)

let parse_window json =
  let open Yojson.Safe.Util in
  try
    let u = json |> member "utilization" |> to_float in
    let r = json |> member "resets_at" |> to_string in
    Some { utilization = u; resets_at = r }
  with _ -> None

let parse_usage_response body =
  try
    let json = Yojson.Safe.from_string body in
    let open Yojson.Safe.Util in
    let w key = try parse_window (json |> member key) with _ -> None in
    {
      five_hour = w "five_hour";
      seven_day = w "seven_day";
      seven_day_sonnet = w "seven_day_sonnet";
      seven_day_opus = w "seven_day_opus";
    }
  with _ -> empty

(* Fetch usage data from the OAuth API *)
let fetch () =
  let* token_opt = read_access_token () in
  match token_opt with
  | None -> Lwt.return_none
  | Some token ->
    Lwt.catch (fun () ->
      let uri = Uri.of_string "https://api.anthropic.com/api/oauth/usage" in
      let headers = Cohttp.Header.of_list [
        "Authorization", "Bearer " ^ token;
        "anthropic-beta", "oauth-2025-04-20";
        "Content-Type", "application/json";
        "Accept", "application/json";
      ] in
      let* resp, body = Cohttp_lwt_unix.Client.get uri ~headers in
      let status = Cohttp.Response.status resp |> Cohttp.Code.code_of_status in
      if status = 200 then begin
        let* body_str = Cohttp_lwt.Body.to_string body in
        Lwt.return_some (parse_usage_response body_str)
      end else begin
        let* _ = Cohttp_lwt.Body.drain_body body in
        Lwt.return_none
      end
    ) (fun _exn -> Lwt.return_none)

(* Format usage for status bar display *)
let format_status_bar usage =
  let fmt_window label w =
    Printf.sprintf "%.0f%% %s" w.utilization label in
  match usage.seven_day, usage.five_hour with
  | Some wk, Some hr -> Printf.sprintf "%s | %s" (fmt_window "wk" wk) (fmt_window "5h" hr)
  | Some wk, None -> fmt_window "wk" wk
  | None, Some hr -> fmt_window "5h" hr
  | None, None -> ""

(* Format usage for /cost command display *)
let format_detailed usage =
  let buf = Buffer.create 128 in
  (match usage.seven_day with
   | Some w -> Buffer.add_string buf
       (Printf.sprintf "Weekly usage: %.1f%% (resets %s)\n" w.utilization w.resets_at)
   | None -> ());
  (match usage.five_hour with
   | Some w -> Buffer.add_string buf
       (Printf.sprintf "5-hour usage: %.1f%% (resets %s)\n" w.utilization w.resets_at)
   | None -> ());
  (match usage.seven_day_sonnet with
   | Some w -> Buffer.add_string buf
       (Printf.sprintf "Sonnet weekly: %.1f%%\n" w.utilization)
   | None -> ());
  (match usage.seven_day_opus with
   | Some w -> Buffer.add_string buf
       (Printf.sprintf "Opus weekly: %.1f%%\n" w.utilization)
   | None -> ());
  let s = Buffer.contents buf in
  if s = "" then "Usage data unavailable" else String.trim s

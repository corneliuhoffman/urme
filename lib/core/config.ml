(* XDG-compliant paths for urme *)

let xdg_data_home () =
  match Sys.getenv_opt "XDG_DATA_HOME" with
  | Some d -> d
  | None -> Filename.concat (Sys.getenv "HOME") ".local/share"

let xdg_config_home () =
  match Sys.getenv_opt "XDG_CONFIG_HOME" with
  | Some d -> d
  | None -> Filename.concat (Sys.getenv "HOME") ".config"

let xdg_cache_home () =
  match Sys.getenv_opt "XDG_CACHE_HOME" with
  | Some d -> d
  | None -> Filename.concat (Sys.getenv "HOME") ".cache"

let data_dir () = Filename.concat (xdg_data_home ()) "urme"
let config_dir () = Filename.concat (xdg_config_home ()) "urme"
let cache_dir () = Filename.concat (xdg_cache_home ()) "urme"

let store_path ~repo_id =
  Filename.concat (data_dir ()) (Filename.concat "stores" repo_id)

let config_file () =
  Filename.concat (config_dir ()) "config.json"

type plan = Pro | Max | Api

type rgb = { r : int; g : int; b : int }

type theme = {
  bg : rgb;
  fg : rgb;
  border : rgb;
  status_bg : rgb;
  input_bg : rgb;
  highlight : rgb;
  user : rgb;
  assistant : rgb;
  thinking : rgb;
  tool_name : rgb;
  tool_result : rgb;
  error : rgb;
  diff_add_fg : rgb;
  diff_add_bg : rgb;
  diff_del_fg : rgb;
  diff_del_bg : rgb;
  separator : rgb;
  selection_bg : rgb;
}

type config = {
  github_token : string option;
  claude_binary : string;
  plan : plan;
  theme : theme;
}

let plan_of_string = function
  | "pro" -> Pro | "max" -> Max | _ -> Api

let rgb r g b = { r; g; b }

(* Parse "#rrggbb" hex string to rgb *)
let rgb_of_hex s =
  let s = if String.length s > 0 && s.[0] = '#' then String.sub s 1 (String.length s - 1) else s in
  if String.length s = 6 then
    { r = int_of_string ("0x" ^ String.sub s 0 2);
      g = int_of_string ("0x" ^ String.sub s 2 2);
      b = int_of_string ("0x" ^ String.sub s 4 2) }
  else { r = 0; g = 0; b = 0 }

(* Tokyo Night theme — matches WezTerm "Tokyo Night" *)
let tokyo_night = {
  bg          = rgb 15 17 26;     (* #0f111a — OLED background *)
  fg          = rgb 169 177 214;  (* #a9b1d6 *)
  border      = rgb 59 66 97;     (* #3b4261 *)
  status_bg   = rgb 36 40 59;     (* #24283b *)
  input_bg    = rgb 26 29 42;     (* #1a1d2a *)
  highlight   = rgb 224 175 104;  (* #e0af68 *)
  user        = rgb 125 207 255;  (* #7dcfff *)
  assistant   = rgb 158 206 106;  (* #9ece6a *)
  thinking    = rgb 86 95 137;    (* #565f89 *)
  tool_name   = rgb 42 195 222;   (* #2ac3de *)
  tool_result = rgb 144 153 192;  (* #9099c0 *)
  error       = rgb 247 118 142;  (* #f7768e *)
  diff_add_fg = rgb 255 255 255;  (* white *)
  diff_add_bg = rgb 0 95 0;       (* 256-palette dark green *)
  diff_del_fg = rgb 255 255 255;  (* white *)
  diff_del_bg = rgb 95 0 0;       (* 256-palette dark red *)
  separator   = rgb 59 66 97;     (* #3b4261 *)
  selection_bg = rgb 42 46 54;    (* #2a2e36 *)
}

let default_config = {
  github_token = None;
  claude_binary = "claude";
  plan = Pro;
  theme = tokyo_night;
}

let load () =
  let path = config_file () in
  if Sys.file_exists path then begin
    let ic = open_in path in
    let content = In_channel.input_all ic in
    close_in ic;
    try
      let json = Yojson.Safe.from_string content in
      let open Yojson.Safe.Util in
      let str key default =
        try json |> member key |> to_string with _ -> default in
      let int_val key default =
        try json |> member key |> to_int with _ -> default in
      let _ = int_val in
      {
        github_token =
          (match Sys.getenv_opt "GITHUB_TOKEN" with
           | Some t -> Some t
           | None ->
             try Some (json |> member "github_token" |> to_string)
             with _ -> None);
        claude_binary = str "claude_binary" "claude";
        plan = plan_of_string (str "plan" "pro");
        theme = (
          let t = try json |> member "theme" with _ -> `Null in
          match t with
          | `Assoc _ ->
            let c key default =
              try rgb_of_hex (t |> member key |> to_string) with _ -> default in
            { bg          = c "bg"          tokyo_night.bg;
              fg          = c "fg"          tokyo_night.fg;
              border      = c "border"      tokyo_night.border;
              status_bg   = c "status_bg"   tokyo_night.status_bg;
              input_bg    = c "input_bg"    tokyo_night.input_bg;
              highlight   = c "highlight"   tokyo_night.highlight;
              user        = c "user"        tokyo_night.user;
              assistant   = c "assistant"   tokyo_night.assistant;
              thinking    = c "thinking"    tokyo_night.thinking;
              tool_name   = c "tool_name"   tokyo_night.tool_name;
              tool_result = c "tool_result" tokyo_night.tool_result;
              error       = c "error"       tokyo_night.error;
              diff_add_fg = c "diff_add_fg" tokyo_night.diff_add_fg;
              diff_add_bg = c "diff_add_bg" tokyo_night.diff_add_bg;
              diff_del_fg = c "diff_del_fg" tokyo_night.diff_del_fg;
              diff_del_bg = c "diff_del_bg" tokyo_night.diff_del_bg;
              separator   = c "separator"   tokyo_night.separator;
              selection_bg= c "selection_bg"tokyo_night.selection_bg;
            }
          | _ -> tokyo_night);
      }
    with _ -> default_config
  end else
    { default_config with
      github_token = Sys.getenv_opt "GITHUB_TOKEN" }

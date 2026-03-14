(* Content-based matching: decompose a git diff into attributed Claude edits *)

open Git_link_types
open Lwt.Syntax

(* Convert a character position to a 1-based line number *)
let char_pos_to_line content pos =
  let line = ref 1 in
  let limit = min pos (String.length content - 1) in
  for i = 0 to limit - 1 do
    if content.[i] = '\n' then incr line
  done;
  !line

(* Get changed line ranges from a patch's hunks *)
let changed_ranges_of_patch patch =
  List.map (fun (hunk : Patch.hunk) ->
    (hunk.their_start, hunk.their_start + hunk.their_len - 1)
  ) patch.Patch.hunks

let in_ranges line ranges =
  List.exists (fun (s, e) -> line >= s && line <= e) ranges

(* Find all positions of needle in haystack *)
let find_all_positions needle haystack =
  let nlen = String.length needle in
  let hlen = String.length haystack in
  if nlen = 0 || nlen > hlen then []
  else begin
    let positions = ref [] in
    let i = ref 0 in
    (try while !i <= hlen - nlen do
      match find_substring needle (String.sub haystack !i (hlen - !i)) with
      | Some rel -> positions := (!i + rel) :: !positions; i := !i + rel + 1
      | None -> i := hlen + 1
    done with _ -> ());
    List.rev !positions
  end

(* When multiple positions match, pick the best one using:
   1. Prefer positions inside diff hunk ranges
   2. Break ties by character agreement with content_before *)
let disambiguate ~content_before ~after_ranges positions cc needle_len =
  match positions with
  | [] -> None
  | [p] -> Some p
  | _ ->
    let scored = List.map (fun pos ->
      let line = char_pos_to_line cc pos in
      let hunk_bonus = if in_ranges line after_ranges then 10000 else 0 in
      let check_start = max 0 (pos - 40) in
      let check_end = min (String.length cc) (pos + needle_len + 40) in
      let cb_len = String.length content_before in
      let char_score = ref 0 in
      for i = check_start to check_end - 1 do
        if i < String.length cc && i < cb_len &&
           cc.[i] = content_before.[i] then
          incr char_score
      done;
      (pos, hunk_bonus + !char_score)
    ) positions in
    let (best_pos, _) = List.fold_left (fun (bp, bs) (p, s) ->
      if s > bs then (p, s) else (bp, bs)
    ) (List.hd scored) (List.tl scored) in
    Some best_pos

(* Match edits against file content by undoing them backwards (newest first).
   Only matches edits that explain the delta from content_before to content_after.
   Uses Patch hunks to disambiguate when the same substring appears at multiple positions.
   Returns matched edits in application order (oldest first). *)
let match_edits ~content_before ~content_after ~commit_ts ~file_base ~branch_filter ~warnings edits =
  let candidates = edits
    |> List.filter (fun e ->
      e.file_base = file_base &&
      e.timestamp < commit_ts &&
      (branch_filter = "" || e.git_branch = branch_filter))
    |> List.sort (fun a b -> Float.compare b.timestamp a.timestamp) in
  if candidates = [] then []
  else
  let patch_opt = Patch.diff
    (Some (file_base, content_before))
    (Some (file_base, content_after)) in
  let after_ranges = match patch_opt with
    | Some patch -> changed_ranges_of_patch patch
    | None -> [] in
  let current = ref content_after in
  let matched = ref [] in
  List.iter (fun (edit : edit) ->
    if !current = content_before then ()
    else if edit.old_string <> "" then begin
      let cc = !current in
      match find_substring edit.new_string cc with
      | None ->
        (* new_string not in current — a human edit may have changed it.
           Find where new_string was in content_after, use surrounding
           context to locate the region in current, extract what replaced it. *)
        let ns = edit.new_string in
        let ctx_len = 60 in
        (match find_substring ns content_after with
        | None ->
          warnings := (Printf.sprintf "MISS %s: not in current or content_after"
            edit.edit_key) :: !warnings
        | Some ca_pos ->
          let before_start = max 0 (ca_pos - ctx_len) in
          let after_end = min (String.length content_after) (ca_pos + String.length ns + ctx_len) in
          let ctx_before = String.sub content_after before_start (ca_pos - before_start) in
          let ctx_after = String.sub content_after (ca_pos + String.length ns)
            (after_end - ca_pos - String.length ns) in
          let cc = !current in
          match find_substring ctx_before cc with
          | None ->
            warnings := (Printf.sprintf "MISS %s: context not found in current"
              edit.edit_key) :: !warnings
          | Some cb_pos ->
            let region_start = cb_pos + String.length ctx_before in
            (match find_substring ctx_after (String.sub cc region_start
              (String.length cc - region_start)) with
            | None ->
              warnings := (Printf.sprintf "MISS %s: end context not found"
                edit.edit_key) :: !warnings
            | Some rel_end ->
              let human_text = String.sub cc region_start rel_end in
              if human_text <> ns then begin
                (* Record the human change, then undo both *)
                let human_edit = { edit with
                  edit_key = edit.edit_key ^ ":human";
                  old_string = ns;
                  new_string = human_text;
                  session_id = "human" } in
                warnings := (Printf.sprintf "HUMAN %s: %d->%d chars"
                  edit.edit_key (String.length ns) (String.length human_text)) :: !warnings;
                (* Replace human_text with old_string in current *)
                current := String.sub cc 0 region_start ^ edit.old_string ^
                  String.sub cc (region_start + rel_end)
                    (String.length cc - region_start - rel_end);
                matched := edit :: human_edit :: !matched
              end))
      | _ ->
        if edit.replace_all then begin
          let positions = find_all_positions edit.new_string cc in
          let matching_positions = match patch_opt with
            | None -> positions
            | Some patch ->
              List.filter (fun pos ->
                let line = char_pos_to_line cc pos in
                let in_hunk = List.find_opt (fun (hunk : Patch.hunk) ->
                  line >= hunk.their_start &&
                  line < hunk.their_start + hunk.their_len
                ) patch.Patch.hunks in
                match in_hunk with
                | None -> false
                | Some hunk ->
                  let mine_text = String.concat "\n" hunk.mine in
                  find_substring edit.old_string mine_text <> None
              ) positions
          in
          let to_replace = match patch_opt with
            | Some _ -> matching_positions
            | None -> positions in
          if to_replace <> [] then begin
            let sorted_desc = List.sort (fun a b -> Int.compare b a) to_replace in
            let result = List.fold_left (fun acc pos ->
              let ns_len = String.length edit.new_string in
              let acc_len = String.length acc in
              String.sub acc 0 pos ^ edit.old_string ^
                String.sub acc (pos + ns_len) (acc_len - pos - ns_len)
            ) cc sorted_desc in
            current := result;
            matched := edit :: !matched
          end
        end else begin
          let positions = find_all_positions edit.new_string cc in
          let hunk_filtered = match patch_opt with
            | Some patch when List.length positions > 1 ->
              List.filter (fun pos ->
                let line = char_pos_to_line cc pos in
                match List.find_opt (fun (hunk : Patch.hunk) ->
                  line >= hunk.their_start &&
                  line < hunk.their_start + hunk.their_len
                ) patch.Patch.hunks with
                | Some hunk ->
                  let mine_text = String.concat "\n" hunk.mine in
                  find_substring edit.old_string mine_text <> None
                | None -> false
              ) positions
            | _ -> positions in
          let use_positions = match patch_opt with
            | Some _ when List.length positions > 1 ->
              hunk_filtered
            | _ -> positions in
          let best = disambiguate ~content_before ~after_ranges use_positions cc
            (String.length edit.new_string) in
          (match best with
          | None -> ()
          | Some pos ->
            let ns_len = String.length edit.new_string in
            let cc_len = String.length cc in
            current := String.sub cc 0 pos ^ edit.old_string ^
                       String.sub cc (pos + ns_len) (cc_len - pos - ns_len);
            matched := edit :: !matched)
        end
    end else if edit.new_string <> "" then begin
      let ns = edit.new_string in
      let cc = !current in
      let check_len = min (String.length ns) (String.length cc) in
      if check_len > 20 then
        let sample = String.sub ns 0 (min 100 (String.length ns)) in
        match find_substring sample cc with
        | Some _ ->
          matched := edit :: !matched
        | None ->
          warnings := (Printf.sprintf "MISS %s: Write content not in current"
            edit.edit_key) :: !warnings
    end
  ) candidates;
  !matched

(* Decompose a single commit's diff for a file into provenance *)
let decompose_diff ~sha ~file ~branch_label ~edits ~cwd ~repo =
  let file_base = Filename.basename file in
  let branch = match Hashtbl.find_opt branch_label sha with
    | Some b -> b | None -> "" in
  let* content_after = Lwt.catch (fun () ->
    Urme_store.Project_store.read_blob ~repo ~sha ~path:file
  ) (fun _ -> Lwt.return_none) in
  match content_after with
  | None -> Lwt.return { commit_sha = sha; file = file_base; items = [] }
  | Some content_after ->
    let* ts_str = Lwt.catch (fun () ->
      Urme_git.Ops.run_git ~cwd ["log"; "-1"; "--format=%at"; sha]
    ) (fun _ -> Lwt.return "0") in
    let commit_ts = try float_of_string (String.trim ts_str) with _ -> 0.0 in
    let* parents_str = Lwt.catch (fun () ->
      Urme_git.Ops.run_git ~cwd ["log"; "-1"; "--format=%P"; sha]
    ) (fun _ -> Lwt.return "") in
    let parents = String.split_on_char ' ' (String.trim parents_str)
      |> List.filter (fun s -> s <> "") in
    let* content_before = match parents with
      | parent :: _ ->
        Lwt.catch (fun () ->
          let+ c = Urme_store.Project_store.read_blob ~repo ~sha:parent ~path:file in
          Option.value c ~default:""
        ) (fun _ -> Lwt.return "")
      | [] -> Lwt.return "" in
    let warnings = ref [] in
    if List.length parents <= 1 then begin
      let matched = match_edits ~content_before ~content_after ~commit_ts ~file_base
        ~branch_filter:branch ~warnings edits in
      let warn_items = List.map (fun w -> Unexplained w) !warnings in
      Lwt.return { commit_sha = sha; file = file_base;
                   items = List.map (fun e -> DirectEdit e) matched @ warn_items }
    end else begin
      let theirs_parent = List.nth parents 1 in
      let theirs_branch = match Hashtbl.find_opt branch_label theirs_parent with
        | Some b -> b | None -> "" in
      let ours = match_edits ~content_before ~content_after ~commit_ts ~file_base
        ~branch_filter:branch ~warnings edits in
      let theirs = match_edits ~content_before ~content_after ~commit_ts ~file_base
        ~branch_filter:theirs_branch ~warnings edits in
      let items =
        List.map (fun e -> DirectEdit e) ours @
        (if theirs <> [] then
           [Incoming (List.map (fun e -> DirectEdit e) theirs, theirs_branch)]
         else []) in
      Lwt.return { commit_sha = sha; file = file_base; items }
    end

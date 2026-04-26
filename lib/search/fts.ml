(* FTS5 query construction + tokenisation.

   The input is whatever the user typed. FTS5 is strict about syntax (quotes,
   parens, operators) so we normalise aggressively: split on whitespace, keep
   alphanumeric/underscore/dot/hyphen runs, drop the rest, then join with
   the AND-implicit space separator. Quoted segments in the user input
   (e.g. "git walk") are preserved as phrase queries.

   If you want OR, pass [~join:`Or]. Default is `And — the query "taint
   propagator ocaml" means all three must be present. *)

type join = And | Or

(* Split the input into tokens, respecting double-quoted phrases. *)
let tokenise input =
  let n = String.length input in
  let buf = Buffer.create 16 in
  let tokens = ref [] in
  let in_quote = ref false in
  let push () =
    if Buffer.length buf > 0 then begin
      tokens := Buffer.contents buf :: !tokens;
      Buffer.clear buf
    end
  in
  for i = 0 to n - 1 do
    let c = input.[i] in
    if c = '"' then begin
      push ();
      in_quote := not !in_quote
    end else if !in_quote then
      Buffer.add_char buf c
    else if c = ' ' || c = '\t' || c = '\n' then
      push ()
    else
      Buffer.add_char buf c
  done;
  push ();
  List.rev !tokens

(* FTS5-safe token: alphanumerics, underscore, dot, hyphen. Anything else
   is stripped. A token that starts non-alphanumeric or becomes empty is
   dropped. *)
let sanitise tok =
  let buf = Buffer.create (String.length tok) in
  String.iter (fun c ->
    match c with
    | 'a'..'z' | 'A'..'Z' | '0'..'9' | '_' | '.' | '-' ->
      Buffer.add_char buf c
    | _ -> ()
  ) tok;
  let s = Buffer.contents buf in
  if s = "" then None else Some s

(* Wrap a token that looks like a phrase (contains a space) in double quotes
   for FTS5. Single tokens go unquoted; FTS5 treats bare tokens as prefix
   matches only if you add *, so we leave them as exact matches. *)
let render_tok tok =
  if String.contains tok ' ' then Printf.sprintf "\"%s\"" tok
  else tok

let build_match ?(join=And) input =
  let toks = tokenise input in
  let cleaned = List.filter_map (fun t ->
    if String.contains t ' ' then Some t  (* phrase, keep as-is *)
    else sanitise t
  ) toks in
  if cleaned = [] then None
  else
    let sep = match join with And -> " " | Or -> " OR " in
    Some (String.concat sep (List.map render_tok cleaned))

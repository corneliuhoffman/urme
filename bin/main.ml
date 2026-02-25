let () =
  Printexc.record_backtrace true;
  let port = ref 8000 in
  let project_dir = ref "." in
  let args = Array.to_list Sys.argv in
  let rec parse = function
    | "--chromadb-port" :: p :: rest ->
      port := int_of_string p;
      parse rest
    | "--project-dir" :: d :: rest ->
      project_dir := d;
      parse rest
    | _ :: rest -> parse rest
    | [] -> ()
  in
  parse (List.tl args);
  Lwt_main.run (Experience_agent.Mcp_server.run ~port:!port ~project_dir:!project_dir)

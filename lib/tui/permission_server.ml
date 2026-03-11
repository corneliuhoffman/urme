open Lwt.Syntax

(* Unix domain socket server for receiving permission prompts from the
   MCP permission bridge. Each connection is one request-response cycle:
   bridge sends {"tool_name":..., "input":...}, we call on_request,
   and send back {"allow": true/false}. *)

type permission_request = {
  tool_name : string;
  tool_input : Yojson.Safe.t;
}

type t = {
  socket_path : string;
  server_fd : Lwt_unix.file_descr;
  cancel : unit Lwt.u;  (* resolve to stop accept loop *)
}

let socket_path () =
  Printf.sprintf "/tmp/urme-perm-%d.sock" (Unix.getpid ())

let path t = t.socket_path

let parse_request line =
  let json = Yojson.Safe.from_string line in
  let open Yojson.Safe.Util in
  {
    tool_name = (try json |> member "tool_name" |> to_string with _ -> "unknown");
    tool_input = (try json |> member "input" with _ -> `Null);
  }

(* Handle a single client connection *)
let handle_client ~on_request client_fd =
  Lwt.catch (fun () ->
    let ic = Lwt_io.of_fd ~mode:Lwt_io.Input client_fd in
    let oc = Lwt_io.of_fd ~mode:Lwt_io.Output client_fd in
    let* line = Lwt_io.read_line ic in
    let req = parse_request line in
    let* allow = on_request req in
    let response = `Assoc ["allow", `Bool allow] in
    let* () = Lwt_io.write_line oc (Yojson.Safe.to_string response) in
    let* () = Lwt_io.flush oc in
    Lwt_unix.close client_fd
  ) (fun _exn ->
    Lwt.catch (fun () -> Lwt_unix.close client_fd) (fun _ -> Lwt.return_unit)
  )

(* Start the permission server. on_request is called for each incoming
   permission prompt and should return true (allow) or false (deny). *)
let start ~on_request () =
  let sp = socket_path () in
  (* Remove stale socket *)
  (try Unix.unlink sp with Unix.Unix_error _ -> ());
  let fd = Lwt_unix.socket Unix.PF_UNIX Unix.SOCK_STREAM 0 in
  let addr = Unix.ADDR_UNIX sp in
  let* () = Lwt_unix.bind fd addr in
  Lwt_unix.listen fd 5;
  let cancel_promise, cancel = Lwt.wait () in
  (* Accept loop in background *)
  Lwt.async (fun () ->
    let rec accept_loop () =
      let accept = Lwt_unix.accept fd |> Lwt.map (fun x -> `Accepted x) in
      let cancel = cancel_promise |> Lwt.map (fun () -> `Cancelled) in
      let* result = Lwt.pick [accept; cancel] in
      match result with
      | `Cancelled -> Lwt.return_unit
      | `Accepted (client_fd, _addr) ->
        Lwt.async (fun () -> handle_client ~on_request client_fd);
        accept_loop ()
    in
    accept_loop ()
  );
  Lwt.return { socket_path = sp; server_fd = fd; cancel }

let stop t =
  Lwt.wakeup_later t.cancel ();
  let* () = Lwt_unix.close t.server_fd in
  (try Unix.unlink t.socket_path with Unix.Unix_error _ -> ());
  Lwt.return_unit

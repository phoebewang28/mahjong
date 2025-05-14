open Lwt.Infix

type client = {
  addr : string;
  port : int;
  out_chan : Lwt_io.output_channel;
}

let clients = ref []

let broadcast msg =
  Lwt_list.iter_p
    (fun c ->
      Lwt.catch
        (fun () -> Lwt_io.write_line c.out_chan msg)
        (fun _ -> Lwt.return_unit))
    !clients

let string_of_sockaddr (addr : Unix.sockaddr) : string =
  match addr with
  | Unix.ADDR_INET (inet_addr, port) ->
      let ip_str = Unix.string_of_inet_addr inet_addr in
      Printf.sprintf "%s:%d" ip_str port
  | Unix.ADDR_UNIX path -> Printf.sprintf "unix:%s" path

let remove_client client =
  clients := List.filter (fun c -> c.out_chan != client.out_chan) !clients

let rec handle_client client input_chan =
  Lwt_io.read_line_opt input_chan >>= function
  | Some msg ->
      if String.starts_with ~prefix:"MOVE:" msg then
        let content = String.sub msg 5 (String.length msg - 5) in
        let full_msg = Printf.sprintf "STATE:%s" content in
        broadcast full_msg >>= fun () -> handle_client client input_chan
      else
        Lwt_io.printf "Unrecognized message from %s:%d: %s\n" client.addr
          client.port msg
        >>= fun () -> handle_client client input_chan
  | None ->
      Lwt_io.printf "Client %s:%d disconnected.\n" client.addr client.port
      >>= fun () ->
      remove_client client;
      Lwt.return_unit

let on_connect addr (input_chan, output_chan) =
  let ip, port =
    match addr with
    | Unix.ADDR_INET (inet_addr, port) ->
        (Unix.string_of_inet_addr inet_addr, port)
    | _ -> ("Unknown", 0)
  in
  Lwt_io.printf "Connection from %s:%d\n" ip port >>= fun () ->
  let client = { addr = ip; port; out_chan = output_chan } in
  clients := client :: !clients;
  handle_client client input_chan

let get_non_loopback_ip () =
  let sock = Unix.socket Unix.PF_INET Unix.SOCK_DGRAM 0 in
  let dummy_addr = Unix.ADDR_INET (Unix.inet_addr_of_string "8.8.8.8", 80) in
  try
    Unix.connect sock dummy_addr;
    let local_addr = Unix.getsockname sock in
    Unix.close sock;
    match local_addr with
    | Unix.ADDR_INET (inet_addr, _) -> Unix.string_of_inet_addr inet_addr
    | _ -> "Unknown"
  with _ ->
    Unix.close sock;
    "Unknown"

let run_server _ port =
  let addr = Unix.ADDR_INET (Unix.inet_addr_any, port) in
  Lwt_io.establish_server_with_client_address addr on_connect >>= fun _server ->
  let actual_ip = get_non_loopback_ip () in
  Lwt_io.printf "Server running at %s:%d\n" actual_ip port >>= fun () ->
  fst (Lwt.wait ())

let run_client ip port =
  Lwt_io.open_connection (Unix.ADDR_INET (ip, port))
  >>= fun (input_chan, output_chan) ->
  let rec loop () =
    Lwt.choose
      [
        ( Lwt_io.read_line_opt Lwt_io.stdin >>= function
          | Some line -> Lwt_io.write_line output_chan ("MOVE:" ^ line)
          | None -> Lwt.return_unit );
        ( Lwt_io.read_line_opt input_chan >>= function
          | Some msg -> Lwt_io.printl msg
          | None ->
              Lwt_io.printl "Server closed connection." >>= fun () -> exit 0 );
      ]
    >>= fun () -> loop ()
  in
  loop ()

let () =
  let usage () =
    Printf.printf "Usage:\n  %s server <port>\n  %s client <IP> <port>\n"
      Sys.argv.(0) Sys.argv.(0)
  in
  if Array.length Sys.argv < 2 then usage ()
  else
    match Sys.argv.(1) with
    | "server" ->
        if Array.length Sys.argv < 3 then usage ()
        else
          let port = int_of_string Sys.argv.(2) in
          Lwt_main.run (run_server Unix.inet_addr_any port)
    | "client" ->
        if Array.length Sys.argv < 4 then usage ()
        else
          let ip = Unix.inet_addr_of_string Sys.argv.(2) in
          let port = int_of_string Sys.argv.(3) in
          Lwt_main.run (run_client ip port)
    | _ -> usage ()

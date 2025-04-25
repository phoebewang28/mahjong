let localhost_5000 = Unix.ADDR_INET (Unix.inet_addr_any, 5000)

let server_ip () =
  let host = Unix.gethostname () in
  let addr = (Unix.gethostbyname host).Unix.h_addr_list.(0) in
  Unix.string_of_inet_addr addr

let ip_for_client =
  Unix.ADDR_INET (Unix.inet_addr_of_string (server_ip ()), 5000)

let get_local_ip () =
  let host = Unix.gethostname () in
  let addresses = Unix.gethostbyname host in
  Array.iter
    (fun addr ->
      let ip_str = Unix.string_of_inet_addr addr in
      Printf.printf "IP address: %s\n" ip_str)
    addresses.Unix.h_addr_list

let string_of_sockaddr = function
  | Unix.ADDR_UNIX s -> s
  | ADDR_INET (ip, port) ->
      Printf.sprintf "%s:%d" (Unix.string_of_inet_addr ip) port

let client_handler client_socket_address (client_in, client_out) =
  let%lwt () =
    Lwt_io.printlf "I got a connection from %s."
      (string_of_sockaddr client_socket_address)
  in
  Lwt.return ()

let run_server () =
  let server () =
    let%lwt () = Lwt_io.printlf "I am the server." in
    get_local_ip ();
    print_endline (Unix.string_of_inet_addr Unix.inet_addr_loopback);
    let%lwt running_server =
      Lwt_io.establish_server_with_client_address localhost_5000 client_handler
    in
    fst (Lwt.wait ())
  in
  Lwt_main.run (server ())

let run_client () =
  let client () =
    let%lwt () = Lwt_io.printlf "I am a client." in
    let%lwt server_in, server_out = Lwt_io.open_connection ip_for_client in
    let%lwt () = Lwt_io.printlf "I connected to the server" in
    Lwt.return ()
  in
  Lwt_main.run (client ())

let _ =
  let print_usage () =
    Printf.printf "Usage: %s <server | client>\n" Sys.argv.(0)
  in
  if Array.length Sys.argv < 2 then print_usage ()
  else
    match Sys.argv.(1) with
    | "server" -> run_server ()
    | "client" -> run_client ()
    | _ -> print_usage ()

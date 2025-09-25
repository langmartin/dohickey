open Cmdliner
open Dohickey_unix

let start listen data =
  if Sys.file_exists data then
    let (ip, port) = match String.split_on_char ':' listen with
    | ip :: port :: _ -> (ip, port)
    | ip :: _ -> (ip, "8080")
    | [] -> ("127.0.0.1", "8080")
    in
    let open Ht_server in
    let _ = start_server ip (int_of_string port) in
    `Ok (Printf.printf "started")
  else
    `Error (false, "data directory must exist")

(* Server options *)
let listen =
  let doc = "listen[:port] to listen for peer and client queries. [port] defaults to 3904. Starts the server." in
  let env = Cmd.Env.info "DOHICKEY_LISTEN" ~doc in
  Arg.(value & opt string "" & info ["listen"] ~env ~docv:"LISTEN" ~doc)

let data =
  let doc = "data directory" in
  let env = Cmd.Env.info "DOHICKEY_DATA" ~doc in
  Arg.(value & opt string "" & info ["data"] ~env ~docv:"DATA" ~doc)

(* Cmd *)
let start_t = Term.(ret (const start $ listen $ data))

let cmd =
  let doc = "run the dohickey server" in
  let man = [
    `S Manpage.s_bugs;
    `P "Email bug reports to <lang.martin@gmail.com>." ]
  in
  let info = Cmd.info "start" ~version:"%‌%VERSION%%" ~doc ~man in
  Cmd.v info start_t

let wait_forever() =
  while true do
    Thread.delay 300.0
  done;
  ()

let main () =
  (match Cmd.eval cmd with
  | 0 ->
    wait_forever();
    0
  | code -> code)
  |> exit

let () = main ()

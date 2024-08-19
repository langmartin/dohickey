let start listen data =
  if listen <> "" && Sys.file_exists data then
    `Ok (Printf.printf "starting")
  else
    `Error (false, "data directory must exist")

open Cmdliner

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

let main () = exit (Cmd.eval cmd)
let () = main ()

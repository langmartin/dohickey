open Indexeddb.Request
open Lwt.Syntax

let the_version = Jv.of_string "v1"

let open_cache () =
  let cs = Jv.get Jv.global "caches" in
  Jv.call cs "open" [| the_version |]
  |> to_lwt

let load_cache cache =
  [|
    "table";
    "main.css";
    "js_client.js";
    "js_service_worker.js"
  |]
  |> Array.map Jv.of_string
  |> Jv.call cache "addAll"
  |> to_lwt

(* let offline_exn req _exn = *)
(*   Console.info ["offline"; req] *)

let to_opt jv = if Jv.is_none jv then None else Some jv

let via_cache req =
  let* caches = open_cache() in
  let* local = Jv.call caches "match" [|req|] |> to_lwt in
  local |> to_opt |> Lwt.return

let put_cache req resp =
  let* c = open_cache() in
  Jv.call c "put" [|req; resp|]
  |> to_lwt

let via_fetch req =
  let* remote = Jv.call Jv.global "fetch" [| req |] |> to_lwt in
  if Jv.is_none remote then
    Lwt.return_none
  else
    let* _ = put_cache req remote in
    Lwt.return_some remote

let default_response =
  let resp = Jv.get Jv.global "Response" in
  let body = Jv.of_string "Networkn't" in
  let status = Jv.of_int 408 in
  let plain = Jv.of_string "text/plain" in
  [| body;
     Jv.obj [|
       "status", status; "headers", Jv.obj [|"content-type", plain|]
     |]
  |]
  |> Jv.new' resp

let handle_fetch ev =
  let respond resp =
    ignore @@
    Jv.call ev "respondWith" [|resp|]
  in
  let req = Jv.get ev "request" in
  let* local = via_cache req in
  match local with
  | Some resp ->
    respond resp;
    Lwt.return_unit
  | None -> let* remote = via_fetch req in
    (match remote with
       | Some remote -> respond remote; Lwt.return_unit
       | None -> respond default_response; Lwt.return_unit)

let add_fetch_listener () = ()

(*
let add_fetch () =
  let trg = Jv.global |> El.of_jv |> El.as_target in
  let fetch = Ev.Type.void (Jstr.v "fetch") in
  ignore @@
  Ev.listen fetch handle_fetch trg
*)

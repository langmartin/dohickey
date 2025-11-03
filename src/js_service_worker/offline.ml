(* open Brr *)
(* open Indexeddb.Request *)

let the_version = Jv.of_string "v1"

(* Turns out Fut and Lwt have similar interfaces *)
open Fut.Result_syntax
let to_lwt jv_promise = Fut.of_promise ~ok:Fun.id jv_promise
let ok x = Fut.ok x
let ok_unit = Fut.ok ()

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

let _to_lwt2 req =
  let result req = Jv.get req "result" in
  let promise, resolver = Lwt.wait () in
  Jv.set req "onsuccess"
  @@ Jv.repr (fun _ev ->
      Brr.Console.debug [ "success", _ev ];
      Jv.set req "onsuccess" Jv.undefined;
      Lwt.wakeup_later resolver @@ result req);
  Jv.set req "onerror"
  @@ Jv.repr (fun _ ->
      Jv.set req "onerror" Jv.undefined;
      let error = Jv.get req "error" in
      Brr.Console.error [ error ];
      Lwt.wakeup_later_exn resolver @@ Jv.Error (Jv.to_error error));
  promise

let debug_cache () =
  let cs = Jv.get Jv.global "caches" in
  let* xs = Jv.call cs "keys" [||] |> to_lwt in
  Brr.Console.debug ["CACHE"; xs];
  ok_unit

let to_opt jv = if Jv.is_none jv then None else Some jv

let via_cache req =
  let* caches = open_cache() in
  let* local = Jv.call caches "match" [|req|] |> to_lwt in
  local |> to_opt |> ok

let put_cache req resp =
  let* c = open_cache() in
  Jv.call c "put" [|req; resp|]
  |> to_lwt

let via_fetch req =
  let* remote = Jv.call Jv.global "fetch" [| req |] |> to_lwt in
  if Jv.is_none remote then
    ok None
  else
    let* _ = put_cache req remote in
    ok (Some remote)

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

  let ev_respond ev resp =
    ignore @@
    Jv.call ev "respondWith" [|resp|]

let handle_install _ev =
  (* Brr.Console.debug ["INST"] *)
  let* cache = open_cache() in
  let* _ = load_cache cache in
  ok_unit

let handle_fetch ev =
  Brr.Console.debug ["TOP"];

  let req = Jv.get ev "request" in
  let respond = ev_respond ev in
  let* local = via_cache req in

  Brr.Console.debug ["LOC"];
  match local with
  | Some resp -> respond resp; ok_unit
  | None ->
    let* remote = via_fetch req in
    (match remote with
       | Some remote -> respond remote; ok_unit
       | None -> respond default_response; ok_unit)

let add_listener event_name f =
  let f = Jv.callback ~arity:1 f in
  ignore @@
  Jv.call Jv.global "addEventListener" [| Jv.of_string event_name; f |]

let add_install_listener () =
  add_listener "install" handle_install

let add_fetch_listener () =
  add_listener "fetch" handle_fetch

(*
let add_fetch () =
  let trg = Jv.global |> El.of_jv |> El.as_target in
  let fetch = Ev.Type.void (Jstr.v "fetch") in
  ignore @@
  Ev.listen fetch handle_fetch trg
*)

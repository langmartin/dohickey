open Brr
open Brr_webworkers

(* Turns out Fut and Lwt have similar interfaces *)
open Fut.Result_syntax
let to_lwt jv_promise = Fut.of_promise ~ok:Fun.id jv_promise
let ok x = Fut.ok x
let ok_unit = Fut.ok ()
let to_promise f = Fut.to_promise ~ok:Jv.repr f

let the_version = Jv.of_string "v1"

let open_cache () =
  let cs = Jv.get Jv.global "caches" in
  Jv.call cs "open" [| the_version |]
  |> to_lwt

let load_cache cache =
  Console.debug ["LOAD"; cache];

  [|
    "table";
    "main.css";
    "js_client.js";
    (* "js_service_worker.js" *)
  |]
  |> Array.map Jv.of_string
  |> Jv.call cache "addAll"
  |> to_lwt

let debug_cache () =
  let cs = Jv.get Jv.global "caches" in
  let* xs = Jv.call cs "keys" [||] |> to_lwt in
  Console.debug ["CACHE"; xs];
  ok_unit

let to_opt jv = if Jv.is_none jv then None else Some jv
let ( >>= ) = Option.bind
let clone resp = Some (Jv.call resp "clone" [||])

let via_cache req =
  let* cache = open_cache() in
  let* local = Jv.call cache "match" [|req|] |> to_lwt in
  local |> to_opt >>= clone |> ok

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

let ev_request ev = Jv.get ev "request"
let ev_respond ev promise = ignore @@ Jv.call ev "respondWith" [|promise|]

let install() =
  ignore @@ Service_worker.G.skip_waiting();
  let* cache = open_cache() in
  let* _ = load_cache cache in
  ok_unit

let fetch req =
  let* local = via_cache req in
  Console.debug ["FETCH"; local];
  match local with
  | Some resp -> ok resp
  | None ->
    let* remote = via_fetch req in
    (match remote with
       | Some remote -> ok remote
       | None -> ok default_response)

let handle pfunc ev =
  (* Console.debug ["HANDLE"; ev]; *)
  ev_request ev
  |> pfunc
  |> to_promise
  |> ev_respond ev

let handle_install ev =
  Console.debug ["HANDLE"; ev];
  let p = install() |> to_promise in
  Jv.call ev "waitUntil" [| p |]

let add_listener event_name f =
  let self = Jv.get Jv.global "self" in
  let f = Jv.callback ~arity:1 f in
  Console.debug ["SELF"; self];
  ignore @@
  Jv.call self "addEventListener" [| Jv.of_string event_name; f |]

let add_install_listener () =
  add_listener "install" handle_install

let add_fetch_listener () =
  add_listener "fetch" (handle fetch)

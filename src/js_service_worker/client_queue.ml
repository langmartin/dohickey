open Brr
open Brr_webworkers
open Js_common

type queued = Req.t

type state = {
  queue : queued Queue.t;
  mutable running : bool;
  mutable client_id : Jstr.t;
}

let state = {
  queue = Queue.create();
  running = false;
  client_id = Jstr.empty;
}

let dequeue() = Queue.take_opt state.queue

let enqueue req = Queue.push req state.queue

let set_client id = state.client_id <- id

open Fut.Result_syntax

let g_post jv =
  let open Service_worker in
  let* cs = Clients.match_all G.clients in
  Console.debug ["CLIENTS"; cs;];
  let f c =
    Console.debug ["POST"; c; jv];
    Client.post c jv
  in
  List.iter f cs;
  Fut.ok ()

let _g_post jv =
  let client_id = state.client_id in
  let open Service_worker in
  let* c = Clients.get G.clients client_id in
  begin
    match c with
    | None -> Console.error ["missing client"; c]
    | Some c -> Client.post c jv
  end;
  Fut.ok ()

let rec drain () =
  match dequeue() with
  | None -> Fut.ok ()
  | Some req ->
    let* _ = req |> Req.to_jv |> g_post in
    drain()

let maybe_start () =
  if state.running then
    ()
  else
    ignore @@
    begin
      state.running <- true;
      let* _ = drain() in
      state.running <- false;
      Fut.ok ()
    end

let user user =
  user |> Req.of_user |> enqueue;
  maybe_start()

let items dims items =
  Console.info ["client_push_items"; List.length items];
  let open Req in
  let dims = of_dims dims in
  let reqs = items |> List.map of_item in
  enqueue dims;
  List.iter enqueue reqs;
  maybe_start()

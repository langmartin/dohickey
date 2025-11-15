open Brr
open Brr_webworkers
open Dohickey
open Js_common

type queued = Req.t

type state = {
  queue : queued Queue.t;
  mutable running : bool;
  mutable ready : bool
}

let state = {
  queue = Queue.create();
  running = false;
  ready = false;
}

let dequeue() = Queue.take_opt state.queue

let enqueue req = Queue.push req state.queue

open Fut.Result_syntax

let g_post jv =
  let open Service_worker in
  let* cs = Clients.match_all G.clients in
  let f c = Client.post c jv in
  List.iter f cs;
  Fut.ok ()

let rec drain () =
  match dequeue() with
  | None -> Fut.ok ()
  | Some req ->
    let* _ = req |> Req.to_jv |> g_post in
    Fut.ok ()

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

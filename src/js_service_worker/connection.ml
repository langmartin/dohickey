open Brr
open Brr_io
open Websocket

type handler = Message.Ev.t Brr.Ev.t -> unit
type message = Jv.t list

type t = {
  mutable table : string;
  mutable recv : handler option;
  mutable ws : Websocket.t option;
  queue : message Queue.t
}

let state = {
  table = "";
  recv = None;
  ws = None;
  queue = Queue.create()
}

let dequeue() = Queue.take_opt state.queue

let send ms =
  let jv = Jv.of_list Fun.id ms in
  let str = Json.encode jv in
  match state.ws with
  | Some ws ->
    if ready_state ws == 1 then
      begin
        Console.info(["socket_send"; str]);
        Websocket.send_string ws str
      end
    else
      Queue.add ms state.queue
  | None ->
    Queue.add ms state.queue

let do_each f xs = List.fold_left (fun _ x -> f x; ()) () xs

let rec drain() =
  match Queue.take_opt state.queue with
  | Some message ->
    send message;
    drain()
  | None ->
    ()

let with_handler event_type handler ws =
  ignore @@
  (ws
   |> as_target
   |> Brr.Ev.listen event_type handler);
  ws

let with_close =
  with_handler Ev.close

let with_mesg handler ws =
  match handler with
  | Some handler ->
    with_handler Message.Ev.message handler ws
  | None ->
    ws

let rec on_close _ev =
  reconnect()
and reconnect() =
  let ws = create Jstr.(v "/a1/socket/" + v state.table)
    |> with_mesg state.recv
    |> with_close on_close
  in
  state.ws <- Some ws;
  drain()

let connect recv table =
  state.table <- table;
  state.recv <- Some recv;
  reconnect()

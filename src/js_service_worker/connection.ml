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

let ping_message = Jv.obj [|"ping", Jv.of_bool true|]

let is_ping ms =
  match ms with
  | [m] -> Jv.equal m ping_message
  | _ -> false

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
      if not (is_ping ms) then
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

let rec app_ping () =
  send [ping_message];
  ping_every()
and ping_every () =
  let set_timeout = Jv.get Jv.global "setTimeout" in
  let f = Jv.callback ~arity:1 app_ping in
  ignore @@ Jv.apply set_timeout Jv.[| f; of_int 5000 |]

(* Receive *)

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
  (* TODO back off *)
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
  app_ping();
  reconnect()

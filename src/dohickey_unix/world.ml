open Dohickey

let the_clock = ref (Hulc.init (Node_id.make_id()))
let the_store = ref Tables.empty
let the_clients : (string, Client.t) Hashtbl.t = Hashtbl.create 20

let do_each f xs =
  Seq.fold_left (fun _ x -> f x; ()) () xs

let broadcast items =
  the_clients
  |> Hashtbl.to_seq_values
  |> do_each (Client.send_to items)

(* This doesn't seem right, don't I mean table ^ user id? A
   reconnection from the same client should replace the old one and
   table clients should be isolated *)

let get_client user =
  Hashtbl.find_opt the_clients user

let add_client client =
  let open Client in
  Hashtbl.replace the_clients client.username client

let stop_client id =
  Hashtbl.remove the_clients id

let send() =
  let t' = Hulc.send !the_clock in
  the_clock := t';
  t'

let recv system_time clock items =
  let open Util_result in
  let open Hulc in
  fold_left_until
    (fun clock (item : Item.t) ->
       parse_safe item.coda.time >>= recv_safe system_time clock)
    clock
    items

let recv_the_clock items =
  match recv (Hlc.time_ms()) !the_clock items with
  | Ok t1 -> the_clock := t1; Ok t1
  | Error e -> Error e

let puts table items =
  match recv_the_clock items with
  | Ok _ ->
    let (store', items') = Tables.puts table items !the_store in
    the_store := store';
    broadcast items'
  | Error e ->
    let open Item in
    let time = send() |> Hulc.sprint in
    let body = Error e in
    broadcast [{coda = { time; user = "system" }; body}]

let gets table =
  Tables.get table !the_store

let tables () =
  Tables.tables !the_store

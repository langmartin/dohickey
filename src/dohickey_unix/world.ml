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

let recv items =
  let t' = items
  |> List.map (fun (item : Item.t) -> item.coda.time |> Hulc.parse_opt)
  |> List.concat_map Option.to_list
  |> List.fold_left Hulc.recv !the_clock
  in
    the_clock := t';
    t'

let puts table items =
  ignore @@ recv items;
  let (store', items') = Tables.puts table items !the_store in
  the_store := store';
  broadcast items'

let gets table =
  Tables.get table !the_store

let tables () =
  Tables.tables !the_store

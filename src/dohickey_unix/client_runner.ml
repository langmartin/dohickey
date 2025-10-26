open Lwt.Syntax

let fetch_items table =
  World.gets table
  |> Dohickey.Table.to_list

let is_ping xs =
  let open Yojson.Safe.Util in
  match xs with
  | [m] -> member "ping" m |> to_bool_option |> Option.is_some
  | _ -> false

let handle_items table items =
  let open Yojson.Safe.Util in
  let yo = Yojson.Safe.from_string items in
  let xs = to_list yo in
  if is_ping xs then
    ()
  else
    xs
    |> Json.of_list_json
    |> World.puts table

let flush_queue client =
  let open Client in
  match take client with
  | Some items -> Json.to_json_str items |> Dream.send client.conn
  | None -> Lwt.return_unit

let start client =
  let client_id = World.add_client client in
  Client.send_to (fetch_items client.table) client;
  let rec loop () =
    let* incoming = Dream.receive client.conn in
    match incoming with
    | Some items ->
      handle_items client.table items;
      let* _ = flush_queue client in
      loop ()
    | None ->
      World.stop_client client_id;
      Dream.close_websocket client.conn
  in
  loop ()

let stop client =
  let open Client in
  let* _ = Dream.close_websocket client.conn in
  client.ready <- false;
  Lwt.return client

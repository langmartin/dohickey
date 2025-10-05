open Brr
open Brr_io
open Brr_webworkers
open Dohickey
open Js_common

type t = {
  mutable lamport : int64;
  mutable table : string;
  mutable user : string;
  mutable ws : Websocket.t option;
  mutable data : Table.t
}

let state = {
  lamport = Int64.of_int 0;
  table = "";
  user = "";
  ws = None;
  data = Table.empty
}

let parse data =
  let res = data |> Json.decode |> Result.to_option in
  match res with
  | Some obj -> obj
  | None -> Jv.null

let socket_send_jv jv_list =
  let jv = Jv.of_list Fun.id jv_list in
  let str = Json.encode jv in
  match state.ws with
  | Some ws ->
    Console.info(["socket_send"; str]);
    Websocket.send_string ws str;
  | None -> ()

let socket_send item =
  match Jv_item.of_item item with
  | None -> ()
  | Some jv -> socket_send_jv [jv]

let client_push_title title =
  let open Req in
  title |> of_title |> to_jv |> Worker.G.post

let client_push_user user =
  let open Req in
  user |> of_user |> to_jv |> Worker.G.post

let client_push_item item =
  let open Req in
  let dims = Table.dims state.data |> of_dims |> to_jv in
  Worker.G.post dims;
  let item = item |> of_item |> to_jv in
  Console.debug(["client_push"; item]);
  Worker.G.post item

let join_item item =
  let data = state.data in
  state.data <- Table.join item data

let send_time() =
  let t = Lamport.send state.lamport in
  state.lamport <- t;
  Lamport.sprint64 t

let recv_time (item : Dohickey.Item.t) =
  match Lamport.parse64 item.coda.time with
  | Some m ->
    let t = Lamport.recv state.lamport m in
    state.lamport <- t
  | None -> ()

let push item =
  client_push_item item;
  socket_send item

let save_push item =
  Db.save_item state.table item;
  push item

let save_client_push item =
  Db.save_item state.table item;
  client_push_item item

let recv_item on_fresh item =
  recv_time item;
  if Table.is_fresh state.data item then
    on_fresh item
  else
    ();
  join_item item

let recv_from_ws e =
  let jv = (Message.Ev.data (Ev.as_type e) : Jstr.t) |> parse in
  let recv jv =
    let item = Jv_item.of_obj_jv jv in
    match item with
    | Some item -> recv_item save_client_push item
    | None -> ()
  in
  ignore @@ Jv.to_list recv jv

(* can't do this until the table name is set from the client *)
let connect_ws () =
  let ws = Websocket.create Jstr.(v "/a1/socket/" + v state.table) in
  ignore (Ev.listen Message.Ev.message recv_from_ws (Websocket.as_target ws));
  state.ws <- Some ws;
  ()

let do_each f xs = List.fold_left (fun _ x -> f x; ()) () xs
let got_db_item = recv_item push

let init_db table =
  let init_db_callback table =
    let open Lwt.Syntax in
    let* xs = Db.load_table table in
    do_each got_db_item xs;
    Lwt.return_unit
  in
  (* Without some delay, writing to the websocket immediately after
     connect_ws() causes the the rest of the function execution,
     including parsing a second db record, to stall without raising an
     exception. This probably wants to be checking the readystate.
  *)
  let set_timeout = Jv.get Jv.global "setTimeout" in
  let f() = init_db_callback table in
  let f = Jv.callback ~arity:1 f in
  ignore @@ Jv.apply set_timeout Jv.[| f; of_int 100 |]

let got_item item =
  let time = send_time() in
  let coda = Dohickey.Coda.({time; user = state.user}) in
  let item = Dohickey.Item.({item with coda}) in
  (* Like recv_item but unconditional, because I know this is fresh, and
     avoiding recv_time. *)
  save_push item;
  join_item item

let got_title title =
  let table = Dohickey.Friendly.dohickey_name title in
  state.table <- table;
  connect_ws();
  init_db table;
  client_push_title title

let got_user user =
  state.user <- user;
  client_push_user user

let rec recv_from_page e =
  let open Js_common in
  let data = Message.Ev.data (Ev.as_type e) |> Ev.to_jv in

  Console.info(["recv_from_page"; data]);

  let req = Req.of_jv data in
  begin
    match req.body with
    | Some Title title ->
      got_title title
    | Some User user ->
      got_user user
    | Some Item item ->
      got_item item
    | Some _ -> ()
    | None -> ()
  end;
  recv_lp()

and recv_lp () =
  let msg = Ev.next Message.Ev.message G.target in
  let _ = Fut.map recv_from_page msg in
  ()

let main () =
  Console.(info ["worker hello"]);
  recv_lp()

let () = main ()

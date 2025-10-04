open Brr
open Brr_io
open Brr_webworkers
open Dohickey
open Js_common

type t = {
  mutable table : string;
  mutable ws : Websocket.t option;
  mutable data : Table.t
}

let state = {
  table = "";
  ws = None;
  data = Table.empty
}

let parse data =
  let res = data |> Json.decode |> Result.to_option in
  match res with
  | Some obj -> obj
  | None -> Jv.null

let socket_send jv_list =
  let jv = Jv.of_list Fun.id jv_list in
  let str = Json.encode jv in
  match state.ws with
  | Some ws ->
    Console.info(["socket_send"; str]);
    Websocket.send_string ws str;
  | None -> ()

let client_push_title title =
  let open Req in
  title |> of_title |> to_jv |> Worker.G.post

let client_push item =
  let open Req in
  let dims = Table.dims state.data |> of_dims |> to_jv in
  Worker.G.post dims;
  let item = item |> of_item |> to_jv in
  Console.debug(["client_push"; item]);
  Worker.G.post item

let put_item item =
  let data = Table.put item state.data in
  state.data <- data

let recv_item item =
  put_item item;
  client_push item

let recv_from_ws e =
  let jv = (Message.Ev.data (Ev.as_type e) : Jstr.t) |> parse in
  let recv jv =
    let item = Jv_item.of_obj_jv jv in
    match item with
    | Some item -> recv_item item
    | None -> ()
  in
  ignore @@ Jv.to_list recv jv

(* can't do this until the table name is set from the client *)
let connect_ws () =
  let ws = Websocket.create Jstr.(v "/a1/socket/" + v state.table) in
  ignore (Ev.listen Message.Ev.message recv_from_ws (Websocket.as_target ws));
  state.ws <- Some ws;
  ()

let got_local_item item =
  let send item =
    match Jv_item.of_item item with
    | None -> ()
    | Some jv -> socket_send [jv]
  in
  put_item item;
  send item;
  client_push item

let got_item item =
  ignore @@ Db.save_item state.table item;
  got_local_item item

let do_each f xs = List.fold_left (fun _ x -> f x; ()) () xs

let init_db table =
  let init_db_callback table =
    let open Lwt.Syntax in
    let* xs = Db.load_table table in
    do_each got_local_item xs;
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

let got_title title =
  let table = Dohickey.Friendly.dohickey_name title in
  state.table <- table;
  connect_ws();
  init_db table;
  client_push_title title

let rec recv_from_page e =
  let open Js_common in
  let data = Message.Ev.data (Ev.as_type e) |> Ev.to_jv in

  Console.info(["recv_from_page"; data]);

  let req = Req.of_jv data in
  begin
    match req.body with
    | Some Title title ->
      got_title title
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

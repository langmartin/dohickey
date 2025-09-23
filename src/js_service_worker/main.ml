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

let dbg label obj =
  Console.(debug ["dbg"; label; obj]);
  obj

let parse data =
  let res = data |> Json.decode |> Result.to_option in
  match res with
  | Some obj -> obj
  | None -> Jv.null

let send item =
  match state.ws with
  | Some ws -> Websocket.send_string ws item
  | None -> ()

let client_push item =
  let open Req in
  let dims = Table.dims state.data |> of_dims |> to_jv in
  Worker.G.post dims;
  let item = item |> of_item |> to_jv |> dbg "client_push" in
  Worker.G.post item

let put_item item =
  let data = Table.put item state.data in
  state.data <- data

let recv_from_ws e =
  let jv = (Message.Ev.data (Ev.as_type e) : Jstr.t) |> parse in
  let item = Jv_item.of_jv jv in
  match item with
  | Some item ->
    put_item item;
    client_push item
  | None -> ()

(* can't do this until the table name is set from the client *)
let connect_ws () =
  let ws = Websocket.create Jstr.(v "/a1/socket/" + v state.table) in
  ignore (Ev.listen Message.Ev.message recv_from_ws (Websocket.as_target ws));
  state.ws <- Some ws;
  ()

let got_item item =
  put_item item;
  (match Jv_item.of_item item with
   | None -> ()
   | Some jv ->
     Jv.to_jstr jv |> send);
  client_push item

let rec recv_from_page e =
  let open Js_common in
  let data = Message.Ev.data (Ev.as_type e) |> Ev.to_jv in

  Console.(debug ["recv_from_page"; data]);

  let req = Req.of_jv data in
  begin
    match req.body with
    | Some Title title ->
      state.table <- title;
      connect_ws()
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
  Console.(debug ["worker hello"]);
  recv_lp()

let () = main ()

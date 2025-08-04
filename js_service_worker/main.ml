open Brr
open Brr_io
open Brr_webworkers
open Dohickey

type t = {
  mutable table : Jstr.t;
  mutable ws : Websocket.t option;
  mutable data : Table.t
}

let state = {
  table = Jstr.empty;
  ws = None;
  data = Table.empty
}

let parse data =
  let res = data |> Json.decode |> Result.to_option in
  match res with
  | Some obj -> obj
  | None -> Jv.null

let send item =
  match state.ws with
  | Some ws -> Websocket.send_string ws item
  | None -> ()

let jsint key data =
  Jv.Jstr.get data key
  |> Jstr.to_int
  |> Option.get

let post_item kind row col =
  match Table.get_pos kind row col state.data with
  | Some item ->
    Worker.G.post item
  | None ->
    Worker.G.post Jv.null

let recv_from_ws e =
  let jv = (Message.Ev.data (Ev.as_type e) : Jstr.t) |> parse in
  let item = Jv_item.of_jv jv in
  match item with
  | Some item ->
    let data = Table.put item state.data in
    state.data <- data
  | None -> ()

let recv_from_page e =
  let data = (Message.Ev.data (Ev.as_type e) : Jstr.t) |> parse in
  let path = Jv.Jstr.get data "path" |> Jstr.to_string in
  match path with
  | "table" ->
    let body = Jv.Jstr.get data "body" in
    state.table <- body

  | "connect" ->
    let ws = Websocket.create Jstr.(v "/a1/socket/" + state.table) in
    ignore (Ev.listen Message.Ev.message recv_from_ws (Websocket.as_target ws));
    state.ws <- Some ws

  | "text" | "vote" ->
    let body = Jv.Jstr.get data "body" in
    send body

  | "dims" ->
    Worker.G.post (Table.dims state.data)

  | "get_text" ->
    let row = jsint "row" data in
    let col = jsint "col" data in
    post_item "text" row col

  | _ -> assert false

let main () =
  Console.(log [str "Worker hello!"]);
  let msg = Ev.next Message.Ev.message G.target in
  let _ = Fut.map recv_from_page msg in
  ()

let () = main ()

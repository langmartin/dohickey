open Brr
open Brr_io
open Brr_webworkers
open Dohickey
open Js_common

type t = {
  mutable hulc : Hulc.t;
  mutable table : string;
  mutable user : string;
  mutable data : Table.t;
  mutable history : Item.t list
}

let state = {
  hulc = Hulc.init "";
  table = "";
  user = "";
  data = Table.empty;
  history = []
}

let parse data =
  let res = data |> Json.decode |> Result.to_option in
  match res with
  | Some obj -> obj
  | None -> Jv.null

let socket_send items =
  items
  |> List.map Jv_item.of_item
  |> Connection.send

let client_post req = req |> Req.to_jv |> Worker.G.post

let client_push_user user =
  let open Req in
  user |> of_user |> client_post

let client_push_items items =
  let open Req in
  Table.dims state.data |> of_dims |> client_post;
  items
  |> List.map of_item
  |> List.iter client_post

let client_push_history items =
  items
  |> List.map Req.history_of_item
  |> List.iter client_post

let join_item item =
  let data = state.data in
  state.data <- Table.join item data

let init_hulc user = Hulc.init (Node_id.make_id user)

let send_time() =
  let open Hulc in
  let t = send state.hulc in
  state.hulc <- t;
  sprint t

let recv_time (item : Dohickey.Item.t) =
  let open Hulc in
  match parse_opt item.coda.time with
  | Some m ->
    let t = recv state.hulc m in
    state.hulc <- t
  | None -> ()

let push items =
  client_push_items items;
  socket_send items

let save_push item =
  Db.save_item state.table item;
  push [item]

let append_history items =
  let hs = List.append items state.history
    |> List.sort Item.compare
    |> Util_list.dedup_right Item.compare
  in
  state.history <- hs

let recv_history items =
  append_history items;
  client_push_history items

let recv_items ?(save=true) items =
  List.iter recv_time items;
  let fresh = List.filter (Table.is_fresh state.data) items in
  let stale = List.filter (Table.is_stale state.data) items in
  if save then
    List.iter (Db.save_item state.table) fresh;
  List.iter join_item fresh;
  client_push_items fresh;
  recv_history stale

let recv_from_ws e =
  let jv = (Message.Ev.data (Ev.as_type e) : Jstr.t) |> parse in
  let items = Jv.to_list Jv_item.obj_to_item jv
    |> List.filter Option.is_some
    |> List.map Option.get
    (* Reduce would be more accurate in case some of these override others. *)
    |> List.filter (Table.is_fresh state.data)
  in
  recv_items items;
  client_push_items items

(* can't do this until the table name is set from the client *)
let connect_ws() =
  Connection.connect recv_from_ws state.table

let init_db table =
  let init_db_callback table =
    let open Lwt.Syntax in
    let* xs = Db.load_table table in
    recv_items ~save:false xs;
    push xs;
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

let got_init table_id =
  state.table <- table_id;
  connect_ws();
  init_db table_id

(* Must be called before got_init to set the HULC *)
let got_user user =
  state.user <- user;
  state.hulc <- init_hulc user;
  client_push_user user

let rec recv_from_page e =
  let open Js_common in
  let data = Message.Ev.data (Ev.as_type e) |> Ev.to_jv in
  Console.info(["recv_from_client"; data]);
  let req = Req.of_jv data in
  begin
    match req.body with
    | Some Init table_id ->
      got_init table_id
    | Some User user ->
      got_user user
    | Some Item item ->
      got_item item
    | Some Dims _ | Some History _ -> ()
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

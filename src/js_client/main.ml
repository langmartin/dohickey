open Brr
open Brr_io
open Brr_webworkers
open Fut.Result_syntax
open Util

(* Start the worker and listen for events *)

let start_service_worker () =
  let open Service_worker.Container in
  let open Service_worker.Registration in
  let c = of_navigator G.navigator in
  let* r = register c (Jstr.v "js_service_worker.js") in
  Console.debug ["REG"; r];
  Fut.ok (active r)

let rec recv_from_worker w ev =
  let data = Message.Ev.data (Ev.as_type ev) |> Ev.to_jv in
  (* Console.info(["recv_from_service_worker"; data]); *)
  let req = Js_common.Req.of_jv data in
  begin
    match req.body with
    | Some (Dims (row, col)) -> Draw.dims (row, col)
    | Some Item item -> Draw.item item
    | Some Init _table_id -> ()
    | Some User user -> Draw.user user
    | None -> ()
  end;
  recv_lp w

and recv_lp w =
  let msg = Ev.next Message.Ev.message (Worker.as_target w) in
  let _ = Fut.map (recv_from_worker w) msg in
  ()

let spawn () =
  let* sw = start_service_worker() in
  begin
    match sw with
    | Some sw ->
      let w = Service_worker.as_worker sw in
      Send.set_worker w;
      recv_lp w
    | None ->
      Console.error ["service worker failed to start"]
  end;
  Fut.ok ()

(* Event handlers for 3 main table buttons *)

let start_vote ev =
  Ev.stop_propagation ev;
  let btn = event_el ev in
  Console.(debug [str "start_vote"]);
  match El.parent btn with
  | None -> ()
  | Some _el -> Send.call "FIXME"; ()

let goal_dims rows cols = (rows + 1, cols)
let option_dims rows cols = (rows, cols + 1)

let dims_handler dims ev =
  Ev.stop_propagation ev;
  let btn = event_el ev in
  match El.parent btn with
  | None -> ()
  | Some _el ->
    let dec n = n - 1 in
    let count qs = qsa qs |> List.length |> dec in
    let rows = count "#dohickey tr" in
    let cols = count "#dh-0 th" in
    Draw.dims (dims rows cols)

let add_option = dims_handler option_dims
let add_goal = dims_handler goal_dims

let edit_title ev =
  Ev.stop_propagation ev;
  let el = event_el ev in
  let orig = El.children el in
  let undo() = El.set_children el orig in
  let txt = El.text_content el |> Jstr.to_string in
  El.set_children el
    [Draw.editable_title undo txt]

let init_table() =
  begin
    match qs1 "#user" with
    | Some el -> el |> El.text_content |> Jstr.to_string |> Send.user;
    | None ->
      Console.error ["Missing #user!"];
      ignore @@ failwith "no_user";
  end;
  G.window |>  Window.location |> Uri.fragment |> Jstr.to_string |> Send.table_id

let main () =
  ignore @@ spawn();
  on_click "#votey" start_vote;
  on_click "#add-option" add_option;
  on_click "#add-goal" add_goal;
  on_click "#title" edit_title;
  set_timeout init_table 10;
  Console.info(["client hello"])

let main_table() = ignore @@ on_load main

let index () =
  Console.info(["client hello"])

let main_index() = ignore @@ on_load index

let () =
  Jv.set Jv.global "main_table" (Jv.callback ~arity:1 main_table);
  Jv.set Jv.global "main_index" (Jv.callback ~arity:1 main_index)

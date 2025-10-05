open Brr
open Brr_io
open Brr_webworkers
open Util

(* Start the worker and listen for events *)

let spawn_worker () = try
    Ok (Worker.create (Jstr.v "/static/js_service_worker.js"))
  with
  | Jv.Error e -> Error e

let rec recv_from_worker w ev =
  let data = Message.Ev.data (Ev.as_type ev) |> Ev.to_jv in
  Console.debug(["recv_from_worker"; data]);
  let req = Js_common.Req.of_jv data in
  begin
    match req.body with
    | Some (Dims (row, col)) -> Draw.dims (row, col)
    | Some Item item -> Draw.item item
    | Some Title title -> Draw.title title
    | Some User user -> Draw.user user
    | None -> ()
  end;
  recv_lp w

and recv_lp w =
  let msg = Ev.next Message.Ev.message (Worker.as_target w) in
  let _ = Fut.map (recv_from_worker w) msg in
  ()

let spawn () =
  match spawn_worker () with
  | Error _e -> ()
  | Ok w ->
    Send.set_worker w;
    recv_lp w

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

let add_handler dims ev =
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

let add_option = add_handler option_dims
let add_goal = add_handler goal_dims

let on_click qs f =
  qsa qs |> List.map (add_ev_listener Ev.click f) |> ignore

let main () =
  spawn();
  on_click "#votey" start_vote;
  on_click "#add-option" add_option;
  on_click "#add-goal" add_goal;
  Send.title "Working Title";
  Send.user "Tworker";
  Console.info(["client hello"])

let () =
  (* Wait for page load *)
  ignore @@
  Fut.bind (Ev.next Ev.load (Window.as_target G.window)) @@
  fun _ev -> main(); Fut.return()

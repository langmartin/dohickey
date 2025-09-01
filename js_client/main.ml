open Brr
open Brr_io
open Brr_webworkers

let spawn_worker () = try
    Ok (Worker.create (Jstr.v "test_worker.js"))
  with
  | Jv.Error e -> Error e

let recv_from_worker e =
  let data = Message.Ev.data (Ev.as_type e) |> Ev.to_jv in
  let req = Js_common.Req.of_jv data in
  match req.body with
  | Some (Dims (row, col)) -> Draw.dims (row, col)
  | Some Item item -> Draw.item item
  | None -> ()

let main () =
  let h1 = El.h1 [El.txt' "Test workers"] in
  let info = El.p [ El.strong [El.txt' "Note."];
                    El.txt' " Doesn't work over the file:// protocol."]
  in
  let view = El.div [] in
  El.set_children (Document.body G.document) [h1; info; view];
  match spawn_worker () with
  | Error e -> El.set_children view [El.p El.[txt (Jv.Error.message e)]]
  | Ok w ->
    let msg = Ev.next Message.Ev.message (Worker.as_target w) in
    let _ = Fut.map (recv_from_worker) msg in
    Worker.post w (Jstr.v "Work!");
    ()

let () = main ()

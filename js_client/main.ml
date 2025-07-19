open Brr
open Brr_io
open Brr_webworkers

let spawn_worker () = try Ok (Worker.create (Jstr.v "test_worker.js")) with
| Jv.Error e -> Error e

let recv_from_worker ~view e =
  let data : Jstr.t = Message.Ev.data (Ev.as_type e) in
  El.set_children view [El.p El.[txt Jstr.(v "Worker says: " + data)]]

(* let query =
  let open Js_of_ocaml in

let draw_header cols =
  let row = Document.find_el_by_id G.document (Jstr.of_string "#table_headers") in
  let kids = List.map (fun col ->
      El.th [El.txt' col.value]
    )
      cols
  in
  El.set_children row kids

let draw_table rows =
*)

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
      let _ = Fut.map (recv_from_worker ~view) msg in
      Worker.post w (Jstr.v "Work!");
      ()

let () = main ()

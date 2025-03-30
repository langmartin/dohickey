open Brr
open Brr_io
open Brr_webworkers

let recv_from_page e =
  let data = (Message.Ev.data (Ev.as_type e) : Jstr.t) in
  match Jstr.to_string data with
  | "Work!" -> Worker.G.post Jstr.(v "Page said: " + data + v " I say Revolt!")
  | _ -> assert false

let main () =
  Console.(log [str "Worker hello!"]);
  let msg = Ev.next Message.Ev.message G.target in
  let _ = Fut.map recv_from_page msg in
  ()

let () = main ()

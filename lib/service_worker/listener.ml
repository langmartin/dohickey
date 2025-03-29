(*
   1. Keep an open websocket
   2. Merge items into the collection
   3. Answer requests from the client
   4. Push updates to the client
 *)

open Js_of_ocaml

let add_listener event f =
  let g = Js_of_ocaml__Js.Unsafe.global in
  Dom.addEventListener g (Dom.Event.make event) f Js._false

let the_store = ref Items.empty

let h_fetch =
  !the_store
  |> Items.values
  |> Items.to_json_str

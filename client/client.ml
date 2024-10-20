let make_id() =
  let open Dohickey.Node_id in
  let open Js_of_ocaml in
  let a : uint8Array t = new%js uint8Array(n) in
  (Js.Unsafe.js_expr "crypto")##getRandomBytes(a);
  let s =
  make_id ~hostname:

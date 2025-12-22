open Dohickey

let the_date = Jv.get Jv.global "Date"

let date_of_int64 n =
  let ms = n |> Int64.to_float |> Jv.of_float in
  let dt = Jv.new' the_date [| ms |] in
  dt

let iso_of_hulc (h : Hulc.t) =
  let dt = date_of_int64 h.time.time in
  Jv.to_string (Jv.call dt "toISOString" [||])

let nice_of_hulc (h : Hulc.t) =
  let dt = date_of_int64 h.time.time in
  let ds = Jv.to_string (Jv.call dt "toUTCString" [||]) in
  let i = String.rindex ds ':' in
  String.sub ds 0 i

let make_uuid () =
  let crypto = Jv.get Jv.global "crypto" in
  Jv.call crypto "randomUUID" [||] |> Jv.to_string

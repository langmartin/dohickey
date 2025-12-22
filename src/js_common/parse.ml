open Brr
open Dohickey

let split_opt = Hulc.split_opt

let (>>=) = Option.bind

let dv_to_hlc dv =
  let open Tarray.Data_view in
  let open Int64 in
  let shift_left bits i = Int64.shift_left i bits in
  let hi = get_uint16_be dv 0 |> of_int |> shift_left 32 in
  let mi = get_uint16_be dv 2 |> of_int |> shift_left 16 in
  let lo = get_uint16_be dv 4 |> of_int in
  let time = logor hi mi |> logor lo in
  let tick = get_uint16_be dv 6 in
  let open Dohickey.Hlc in
  Some {time; tick}

let hlc64 s : Hlc.t option =
  let uia = Jv.get Jv.global "Uint8Array" in
  let opts = Jv.obj [|"alphabet", Jv.of_string "base64url"|] in
  let arr = Jv.call uia "fromBase64" [|Jv.of_string s; opts|] in
  let buf = Tarray.buffer (Tarray.of_jv arr) in
  let dv = Tarray.Data_view.of_buffer buf in
  dv_to_hlc dv

let hulc64 serialized =
  let make ({time; node} : Hulc.split) : Hulc.t option =
    match hlc64 time with
      None -> None
    | Some time -> Some {time; node}
  in
  serialized
  |> split_opt
  >>= make

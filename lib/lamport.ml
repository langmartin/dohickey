open Int64
open Bytes

type clock = int64

let one = of_int 1

let send clock = add clock one

let recv clock remote =
  max clock remote |> add one

let sprint64 clock =
  let buf = make 8 '0' in
  set_int64_be buf 0 clock;
  Base64.encode_string ~pad:false (to_string buf)

let parse64 serialized =
  match Base64.decode ~pad:false serialized with
  | Ok str ->
    let buf = of_string str in
    Some (get_int64_be buf 0)
  | _error -> None

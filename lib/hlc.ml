type t = {
     time : int64;
     tick : int
   }

let zero = { time = 0L; tick = 0 }

let time_ms() =
  Unix.gettimeofday() |> ( *. ) 1000. |> Int64.of_float

let init system =
  { time = system; tick = 0 }

let send system local =
  let open Int64 in
  let logical = max system local.time in
  {
    time = logical;
    tick =
      if logical == local.time then
        local.tick + 1
      else
        0
  }

let recv system local remote =
  let open Int64 in
  let logical = max system local.time |> max remote.time in
  {
    time = logical;
    tick =
      if logical == remote.time then
        remote.tick + 1
      else if logical == local.time then
        local.tick + 1
      else
        0
  }

let parse16 serialized =
  let open String in
  {
    time = "0x" ^ sub serialized 0 12 |> Int64.of_string;
    tick = "0x" ^ sub serialized 12 4 |> int_of_string
  }

let sprint16 clock =
  let open Printf in
  sprintf "%012Lx%04x" clock.time clock.tick

let sprint64 clock =
  let buf = Bytes.make 8 '0' in
  let lower_bits = clock.time |> Int64.to_int32 in
  let upper_bits = Int64.shift_right clock.time 32 |> Int64.to_int in
  Bytes.set_int32_be buf 2 lower_bits;
  Bytes.set_int16_be buf 0 upper_bits;
  Bytes.set_int16_be buf 6 clock.tick;
  Base64.encode_string ~pad:false (Bytes.to_string buf)

let parse64_opt serialized =
  match Base64.decode ~pad:false serialized with
  | Ok str ->
    let buf = Bytes.of_string str in
    let lower_bits = Bytes.get_int32_be buf 2 |> Int64.of_int32 in
    let upper_bits = Bytes.get_int16_be buf 0 in
    let time = Int64.shift_left (Int64.of_int upper_bits) 32 |> Int64.logor lower_bits in
    let tick = Bytes.get_int16_be buf 6 in
    Some {time = time; tick = tick}
  | _error -> None

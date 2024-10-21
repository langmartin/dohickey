let b64_of_int32 i =
  let buf = Bytes.make 4 '0' in
  Bytes.set_int32_be buf 0 i;
  Base64.encode_string ~pad:false (Bytes.to_string buf)

let crc32_of str =
  let open Checkseum in
  let sum = Crc32.digest_string str 0 (String.length str) in
  sum Crc32.default |> Optint.to_int32 |> b64_of_int32

let time_str unix_time =
  unix_time |> Int32.of_float |> b64_of_int32

let make_id ?(unix_time = Unix.time()) ?(hostname = Unix.gethostname() |> crc32_of) () =
  let host = crc32_of hostname in
  let time = time_str unix_time in
  host ^ time

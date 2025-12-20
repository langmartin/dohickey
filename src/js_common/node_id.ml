let unix_time () =
  let date = Jv.get Jv.global "Date" in
  Jv.call date "now" [||] |> Jv.to_int

let make_id ?(time = unix_time()) user =
  let t = Int.to_string time in
  let lu = String.length user |> min 6 in
  let host = String.sub user 0 lu in
  let time =
    let lt = String.length t in
    let len = 12 - lu in
    String.sub t (lt - len) len
  in
  host ^ time

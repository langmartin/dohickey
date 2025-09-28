open Dohickey.Coda
open Dohickey.Item

let _jf key default coerce jv =
  match Jv.find jv key with
  | Some jv -> coerce jv
  | None -> default

let jint key jv = Jv.Int.get jv key
let jstr key jv = Jv.Jstr.get jv key |> Jstr.to_string
let jobj key jv = Jv.get jv key

let coda_of_jv jv =
  let time = jstr "time" jv in
  let user = jstr "user" jv in
  {time; user}

let of_jv jv =
  let coda = Jv.get jv "coda" |> coda_of_jv in
  let body = Jv.get jv "body" in
  match jstr "type" jv with
  | "text" ->
    let row = jint "row" body in
    let col = jint "col" body in
    let text = jstr "text" body in
    Some {coda; body = Text {row; col; text}}
  | _ -> None

let jv_of_coda coda =
  Jv.obj [|
    ("time", Jv.of_string coda.time);
    ("user", Jv.of_string coda.user)
  |]

let of_item item =
  let coda = jv_of_coda item.coda in
  match item.body with
  | Text i ->
    let body = Jv.obj [|
        ("row", Jv.of_int i.row);
        ("col", Jv.of_int i.col);
        ("text", Jv.of_string i.text)
      |] in
    Some (Jv.obj [|
        ("coda", coda);
        ("body", body);
        ("type", Jv.of_string "text")
      |])

  | _ -> None

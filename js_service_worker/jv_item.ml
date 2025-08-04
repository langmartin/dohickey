open Dohickey.Item

let jint key jv = Jv.Int.get jv key
let jstr key jv = Jv.Jstr.get jv key |> Jstr.to_string
let jobj key jv = Jv.get jv key

let coda_of_jv jv =
  let time = jstr "time" jv in
  let user = jstr "user" jv in
  Dohickey.Coda.({time; user})

let of_jv jv =
  let coda = jobj "coda" jv |> coda_of_jv in
  match jstr "type" jv with
  | "text" ->
    let row = jint "row" jv in
    let col = jint "col" jv in
    let text = jstr "text" jv in
    Some {coda; body = Text {row; col; text}}
  | _ -> None

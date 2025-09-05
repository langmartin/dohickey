let item item =
  let open Js_common.Req in
  let body = Some (Item item) in
  let req = {path = "item"; body} in
  let jv = to_jv req in
  ignore jv

let text row col text =
  let open Dohickey in
  let body = Item.Text {row; col; text} in
  let coda = Coda.empty in
  item {coda; body}

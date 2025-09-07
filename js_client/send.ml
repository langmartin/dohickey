open Brr_webworkers

let the_worker = ref (Worker.of_jv Jv.null)

let worker = !the_worker
let set_worker w = the_worker := w

let post_item item =
  let open Js_common.Req in
  let body = Some (Item item) in
  let req = {path = "item"; body} in
  let jv = to_jv req in
  Worker.post (worker) jv

let text text_body =
  let open Dohickey in
  let coda = Coda.empty in
  let body = Item.Text text_body in
  post_item {coda; body}

open Brr_webworkers

let the_worker = ref (Worker.of_jv Jv.null)
let set_worker w = the_worker := w

let the_user = ref ""
let set_user u = the_user := u

let debug_post jv =
  Brr.Console.(debug ["to worker:"; jv]);
  Worker.post !the_worker jv

let post_item item =
  let open Js_common.Req in
  let body = Some (Item item) in
  let req = {path = "item"; body} in
  let jv = to_jv req in
  debug_post jv

let text text_body =
  let open Dohickey in
  let coda = Coda.empty in
  let body = Item.Text text_body in
  post_item {coda; body}

let vote vote_body =
  let open Dohickey in
  let coda = Coda.empty in
  let body = Item.Vote vote_body in
  post_item {coda; body}

let call id =
  let open Dohickey in
  let coda = Coda.empty in
  let body = Item.Call {id} in
  post_item {coda; body}

let title text =
  let open Dohickey in
  let coda = Coda.empty in
  let body = Item.Title text in
  post_item {coda; body}

let table_id id =
  let open Js_common.Req in
  id
  |> of_init
  |> to_jv
  |> debug_post

let user user =
  let open Js_common.Req in
  user
  |> of_user
  |> to_jv
  |> debug_post

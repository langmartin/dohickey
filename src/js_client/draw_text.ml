open Brr
open Util
open Js_common.Domplate
open Dohickey

let entry_el is_header =
  let qs = if is_header then "#header-template" else "#cell-template" in
  match qs1 qs with
    None -> El.div []
  | Some el -> clone el

let item_text (body : Text.t) (coda : Coda.t) =
  let _user = coda.user in
  let id = Draw_common.cell_id body.row body.col in
  match id.qs ^ " .text" |> qs1 with
  | Some el ->
    El.set_children el
      [El.txt' body.text]
  | None -> ()

let set_el_text row col el =
  let content text =
    El.set_children el
      [El.txt' text];
  in
  (if row = 0 then
     "Option #" ^ (Int.to_string col)
   else if col = 0 then
     "Goal #" ^ (Int.to_string row)
   else
     "deets")
  |> content

let lemme_edit row col ev =
  Ev.prevent_default ev;
  Ev.stop_propagation ev;
  Draw_editor.edit_cell row col

let td_text row col el =
  match qs1 ~el:el ".text" with None -> el | Some elt ->
    set_el_text row col elt;
    ignore @@ el_on_click (lemme_edit row col) elt;
    el

let is_header row col = row = 0 || col = 0

let make_cell row col =
  let id = Draw_common.cell_id row col in
  entry_el (is_header row col)
  |> set_at "id" (Some id.id)
  |> td_text row col

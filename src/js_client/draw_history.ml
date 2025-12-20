open Brr
open Util
open Js_common
open Js_common.Domplate

let entry_el () =
  match qs1 "#hist-item-template" with
    None -> El.div []
  | Some el -> clone el

let id ts = "hist-" ^ ts

let set_ts ts el =
  El.set_at (Jstr.v "id") (Some (Jstr.v (id ts))) el

open Dohickey

let entry (item : Item.t) =
  let ts = item.coda.time in
  let time = Parse.hulc64 item.coda.time |> Option.get |> Format.nice_of_hulc in
  let author = item.coda.user in
  let type' = Item.key item in
  let text = Item.text item in
  let el = entry_el() in
  set_ts ts el;
  data_key_text ("type", type') el;
  data_key_text ("text", text) el;
  data_key_text ("time", time) el;
  data_key_text ("author", author) el;
  el

let find_subsequent ord ts ul =
  let f li = match El.at (Jstr.v "id") li with
      None -> false
    | Some li_ts ->
      let op = if ord = "desc" then ( < ) else ( > ) in
      op (String.compare (Jstr.to_string li_ts) ts) 0
  in
  El.children ~only_els:true ul |> List.find_opt f

let insert_entry (item : Item.t) ul =
  let ts = id item.coda.time in
  let ord = El.at (Jstr.v "data-ord") ul |> Option.get |> Jstr.to_string in
  let el = entry item in
  match find_subsequent ord ts ul with
    None -> El.append_children ul [el]
  | Some next -> El.insert_siblings `Before next [el]

let item (item : Item.t) =
  match qs1 "#history" with
    None -> ()
  | Some ul ->
    let el = qs1 ("#" ^ (id item.coda.time)) in
    if Option.is_some el then
      ()
    else
      insert_entry item ul

open Brr

let data_key_text (key, txt) el =
  let k = "[data-key=" ^ key ^ "]" |> Jstr.of_string in
  match El.find_first_by_selector ~root:el k with None -> None | Some span ->
    El.set_children span [El.txt' txt];
    Some el

let data_keys keys el =
  List.fold_left (fun elo kv ->
      match elo with None -> None | Some el ->
        data_key_text kv el
    )
    (Some el)
    keys

let maker el =
  match El.tag_name el |> Jstr.to_string with
  | "li" -> El.li
  | "span" -> El.span
  | "tr" -> El.tr
  | "th" -> El.th
  | "td" -> El.td
  | "form" -> El.form
  | "select" -> El.select
  | "option" -> El.option
  | _ -> El.div

let attr_names jv =
  Jv.call jv "getAttributeNames" [||]
  |> Jv.to_list Jv.to_jstr
  |> List.filter (fun name -> name <> (Jstr.v "id"))

let rec clone_dom el =
  let el_el = maker el in
  let jv = El.to_jv el in
  let copy = el_el (El.children el |> List.map clone) in
  (* Duplicate attributes *)
  attr_names jv
  |> List.iter (fun n -> El.set_at n (El.at n el) copy);
  (* Remove the template class *)
  El.set_class (Jstr.v "template") false copy;
  copy
and clone el =
  if El.is_txt el then
    El.txt (El.text_content el)
  else
    clone_dom el

let clone_id id =
  match El.find_first_by_selector (Jstr.of_string id) with
    None -> invalid_arg "missing template"
  | Some el -> clone el

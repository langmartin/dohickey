open Brr

let data_key_text (key, txt) el =
  let k = "[data-key=" ^ key ^ "]" |> Jstr.of_string in
  match El.find_first_by_selector ~root:el k with
    None -> ()
  | Some span -> El.set_children span [El.txt' txt]

let maker el =
  match El.tag_name el |> Jstr.to_string with
  | "li" -> El.li
  | "span" -> El.span
  | "tr" -> El.tr
  | "td" -> El.td
  | _ -> El.div

let rec clone_dom el =
  let el_el = maker el in
  let jv = El.to_jv el in
  let names = Jv.call jv "getAttributeNames" [||] |> Jv.to_list Jv.to_jstr in
  let copy = el_el (El.children el |> List.map clone) in
  List.iter (fun n ->
      El.set_at n (El.at n el) copy)
    names;
  El.set_class (Jstr.v "template") false copy;
  copy

and clone el =
  if El.is_txt el then
    El.txt (El.text_content el)
  else
    clone_dom el

open Brr
open Util
open Js_common
open Js_common.Domplate
open Dohickey
open Dohickey.Util_option

let entry_el () =
  match qs1 "#hist-item-template" with
    None -> None
  | Some el -> Some (clone el)

let id ts = "hist-" ^ ts

let set_ts ts el =
  El.set_at (Jstr.v "id") (Some (Jstr.v (id ts))) el;
  Some el

let text_entry (item : Item.t) =
  let time = Parse.hulc64 item.coda.time |> Option.get |> Format.nice_of_hulc in
  entry_el()
  >>= set_ts item.coda.time
  >>= data_keys [
    ("type", Item.key item);
    ("text", Item.text_content item);
    ("time", time);
    ("author", item.coda.user)
  ]

let entry (item : Item.t) =
  match item.body with
  | Text _ -> text_entry item
  | _ -> None

let find_subsequent ord ts ul =
  let f li = match El.at (Jstr.v "id") li with None -> false | Some li_ts ->
    let op = if ord = "desc" then ( < ) else ( > ) in
    let ts0 = Jstr.to_string li_ts in
    let r = op (String.compare ts0 ts) 0 in
    (* Console.debug ["ORD"; ord; ts0; ts; r]; *)
    r
  in
  El.children ~only_els:true ul |> List.find_opt f

let insert_entry (item : Item.t) ul =
  let ts = id item.coda.time in
  let ord = El.at (Jstr.v "data-ord") ul |> Option.get |> Jstr.to_string in
  entry item
  |>> (fun el ->
      match find_subsequent ord ts ul with
      | None -> El.append_children ul [el]
      | Some next -> El.insert_siblings `Before next [el]
    )
  |> ignore

let item (item : Item.t) =
  match qs1 "#history ul" with
    None -> ()
  | Some ul ->
    let el = qs1 ("#" ^ (id item.coda.time)) in
    if Option.is_some el then
      ()
    else
      insert_entry item ul

let init() =
  let arm qs f =
    match qs1 (qs ^ "[data-inert]") with None -> () | Some el ->
      ignore @@ add_ev_listener Ev.click f el
  in
  let cls on off = fun ev ->
    Ev.stop_propagation ev;
    match qs1 "#history" with None -> () | Some el ->
      Console.debug [ev, el];
      El.set_class (Jstr.v off) false el;
      El.set_class (Jstr.v on) true el
  in
  arm "#history-close" (cls "closed" "open");
  arm "#history-open" (cls "open" "closed");

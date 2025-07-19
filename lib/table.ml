module StringMap = Map.Make(String)
type items = Item.t StringMap.t

type t = {
  id: string;
  items: items
}

let empty = {id = ""; items = StringMap.empty}

let make id = {empty with id = id}

let values t =
  t.items |> StringMap.to_list |> List.map snd

let put_item item m =
  let open StringMap in
  let key = Item.key item in
  match find_opt key m with
  | None -> add key item m
  | Some prev ->
    if Coda.compare prev.coda item.coda < 0 then
      add key item m
    else
      m

let put item t = {t with items = (put_item item t.items)}

let put_items items m =
  List.fold_left (fun (m, l) item ->
      let m' = put_item item m in
      if m == m' then
        (m, l)
      else
        (m', item :: l))
    (m, [])
    items

(** reduce [items] into table [t] returning the updated table and subset
    of the items that are fresh by [coda] and the table *)
let puts items t =
  let m, fresh = put_items items t.items in
  ({t with items = m}, fresh)

exception EmptyList

let to_node_id (items : Item.t list) =
  match items with
  | [] -> raise EmptyList
  | item :: _ ->
    match String.split_on_char '-' item.coda.time with
    | _time :: node_id :: [] -> node_id
    | _ -> raise EmptyList

(* Cleanup the names *)

let upper_ascii = "ABCDEFGHIJKLMNOPQRSTUVWXYZ"
let lower_ascii = "abcdefghijklmnopqrstuvwxyz"
let numeric = "0123456789"
let other_ascii = "_-"

let safe_char c =
  if String.contains upper_ascii c ||
     String.contains lower_ascii c ||
     String.contains numeric c ||
     String.contains other_ascii c
  then
    c
  else if c = ' ' then
    '-'
  else
    ' '

let dohickey_name input =
  input
  |> String.lowercase_ascii
  |> String.map safe_char
  |> String.split_on_char ' '
  |> List.filter (fun s -> String.length s <> 0)
  |> String.concat ""

open Dohickey

module StringMap = Map.Make(String)
let empty : Table.t StringMap.t = StringMap.empty

let tables store =
  StringMap.to_list store |> List.map fst

let get table store =
  let open StringMap in
  match find_opt table store with
  | None -> Dohickey.Table.make table
  | Some items -> items

let puts table items store =
  let tab = get table store in
  let items' = List.filter (Table.is_fresh tab) items in
  let tab' = Table.put_list items' tab in
  (StringMap.add table tab' store, items')

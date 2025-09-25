module StringMap = Map.Make(String)
let empty : Dohickey.Table.t StringMap.t = StringMap.empty

let get table store =
  let open StringMap in
  match find_opt table store with
  | None -> Dohickey.Table.empty
  | Some items -> items

let puts table items store =
  let tab = get table store in
  let (itemm', items') = Dohickey.Table.puts items tab in
  (StringMap.add table itemm' store, items')

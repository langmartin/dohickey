module StringMap = Map.Make(String)

let empty : Items.t StringMap.t = StringMap.empty

let get table store =
  let open StringMap in
  match find_opt table store with
  | None -> Items.empty
  | Some items -> items

let put table item store =
  let items = get table store in
  let items' = Items.put item items in
  StringMap.add table items' store

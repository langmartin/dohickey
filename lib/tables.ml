module StringMap = Map.Make(String)

let empty : Items.t StringMap.t = StringMap.empty

let get table store =
  let open StringMap in
  match find_opt table store with
  | None -> Items.empty
  | Some items -> items

let puts table items store =
  let itemm = get table store in
  let (itemm', items') = Items.puts items itemm in
  (StringMap.add table itemm' store, items')

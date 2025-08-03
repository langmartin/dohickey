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

let dims t =
  let max_dims _key item dims =
    let open Item in
    let (r, c) = dims in
    match item.body with
    | Text i -> (max r i.row, max c i.col)
    | _ -> dims
  in
  StringMap.fold max_dims t.items (0, 0)

let get_votes t =
  let open List in
  t.items
  |> StringMap.to_list
  |> map snd
  |> filter Item.is_vote

let comp_pos x y =
  compare (Item.pos_of x) (Item.pos_of y)

let group_votes votes =
  let grp x y = Item.pos_of x == Item.pos_of y in
  let rec lp xs =
    match xs with
    | [] -> [[]]
    | x :: _ ->
      let g = List.take_while (grp x) xs in
      let tail = List.drop_while (grp x) xs in
      g :: (lp tail)
  in
  lp votes

let is_pos n = n > -1

let count_grp xs =
  let open List in
  let rs = xs |> map Item.rank_of |> filter is_pos in
  let sum = List.fold_left ( + ) 0 rs in
  Int.div sum (length rs)

let result_of_grp time user grp =
  match grp with
  | [] -> None
  | x :: _ ->
    let open Item in
    let id = id_of x in
    let (row, col) = pos_of x in
    let rank = count_grp grp in
    let body = Result{row; col; rank; id = id} in
    Some {coda = {time; user}; body}

let make_results time user vote_id t =
  let open List in
  t
  |> get_votes
  |> filter (fun v -> vote_id = Item.id_of v)
  |> sort comp_pos
  |> group_votes
  |> map (result_of_grp time user)
  |> filter Option.is_some
  |> map Option.get

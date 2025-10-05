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

let replacing item t =
  let open StringMap in
  let key = Item.key item in
  match find_opt key t.items with
  | None -> None
  | Some prev ->
    if Coda.compare prev.coda item.coda < 0 then
      Some prev
    else
      None

let is_stale t item =
  let open StringMap in
  let key = Item.key item in
  match find_opt key t.items with
  | None -> false
  | Some prev -> Coda.compare prev.coda item.coda > 0

let is_fresh t item = not (is_stale t item)

let put_list items t =
  let open StringMap in
  let items = List.fold_left (fun m i ->
      let key = Item.key i in
      add key i m)
    t.items
    items
  in
  {t with items}

let join item t =
  let key = Item.key item in
  if is_stale t item then
    t
  else
    {t with items = (StringMap.add key item t.items)}

let dims t =
  let max_dims _key item dims =
    let open Item in
    let (r, c) = dims in
    match item.body with
    | Text i -> (max r i.row, max c i.col)
    | _ -> dims
  in
  StringMap.fold max_dims t.items (0, 0)

let get_pos kind row col t =
  let key = Item.key_pos kind row col in
  StringMap.find_opt key t.items

(*
 * let get_text row col t = get_pos "text"
 * let get_vote row col t = get_pos "vote"
 *)

let to_list t = StringMap.to_list t.items |> List.map snd

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

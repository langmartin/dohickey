type body =
  | Text of {row: int; col: int; text: string}
  | Call of {id: string}
  | Count of {id: string}
  | Vote of Vote.t
  | Result of Vote.t

type t = {coda: Coda.t; body: body}

let key item =
  let open Format in
  match item.body with
  | Text item -> sprintf "text-%d%d" item.row item.col
  | Vote item -> sprintf "vote-%d%d" item.row item.col
  | Result item -> sprintf "result-%d%d" item.row item.col
  | Call item -> sprintf "call-%s" item.id
  | Count item -> sprintf "count-%s" item.id

let is_vote item =
  match item.body with
  | Vote _ -> true
  | _ -> false

let pos_of item =
  match item.body with
  | Text i -> (i.row, i.col)
  | Vote i -> (i.row, i.col)
  | Result i -> (i.row, i.col)
  | _ -> (-1, -1)

let rank_of item =
  match item.body with
  | Vote i -> i.rank
  | Result i -> i.rank
  | _ -> -1

let id_of item =
  match item.body with
  | Vote i -> i.id
  | Result i -> i.id
  | _ -> ""

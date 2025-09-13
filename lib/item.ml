type text_body = {row: int; col: int; text: string}

type call_body = {id: string}

type body =
  | Text of text_body
  | Call of call_body
  | Count of call_body
  | Vote of Vote.t
  | Result of Vote.t

type t = {coda: Coda.t; body: body}

let key_pos word row col = [word; Int.to_string row; Int.to_string col] |> String.concat "-"
let key_id word id = [word; id] |> String.concat "-"

let key_text = key_pos "text"
let key_vote = key_pos "vote"
let key_result = key_pos "result"
let key_call = key_id "call"
let key_count = key_id "count"

let parse_pos word key =
  match key |> String.split_on_char '-' with
  | [pfix; row; col] when pfix = word ->
    Some {row = int_of_string row; col = int_of_string col; text = ""}
  | _ ->
    None

let key item =
  match item.body with
  | Text item -> key_text item.row item.col
  | Vote item -> key_vote item.row item.col
  | Result item -> key_result item.row item.col
  | Call item -> key_call item.id
  | Count item -> key_count item.id

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

type body =
  | Text of Text.t
  | Count of bool
  | Vote of Vote.t
  | Result of Vote.t
  | Title of string
  | Error of string

type t = {coda: Coda.t; body: body}

let key_pos word row col = [word; Int.to_string row; Int.to_string col] |> String.concat "-"
let key_text = key_pos "text"
let key_vote = key_pos "vote"
let key_result = key_pos "result"

let parse_pos word key =
  match key |> String.split_on_char '-' with
  | [pfix; row; col] when pfix = word ->
    Some Text.{row = int_of_string row; col = int_of_string col; text = ""}
  | _ ->
    None

let key item =
  match item.body with
  | Text item -> key_text item.row item.col
  | Vote item -> key_vote item.row item.col
  | Result item -> key_result item.row item.col
  | Count _ -> "count"
  | Title _ -> "title"
  | Error _ -> "error"

let text_key_of_vote item =
  match item.body with
  | Vote b -> key_text b.row b.col
  | _ -> ""

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

let of_title title user time =
  let body = Title title in
  {coda = {user; time}; body}

let compare a b = Coda.compare a.coda b.coda

let text_content item =
  match item.body with
    Text t -> t.text
  | Vote _ -> "Vote"
  | Result _ -> "Result"
  | Count _ -> "Count vote"
  | Title t -> t
  | Error e -> e

type table_id = {
  row : int;
  col : int;
  id : string;
  qs : string
}

let row_id row =
  let id = "dh-" ^ (Int.to_string row) in
  { id; qs = "#" ^ id; row; col = 0 }

let cell_id row col =
  let open List in
  let id = [row; col] |> map Int.to_string |> cons "dh" |> String.concat "-" in
  { id; qs = "#" ^ id; row; col }

let parse_id id_str =
  match String.split_on_char '-' id_str with
  | ["dh"; row; col] -> Some (cell_id (int_of_string row) (int_of_string col))
  | ["dh"; row] -> Some (row_id (int_of_string row))
  | _ -> None

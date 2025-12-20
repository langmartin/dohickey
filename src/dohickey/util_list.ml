open List

let dedup_left compare list =
  let rec lp compare acc list =
    match list with
    | [] -> []
    | [x] -> [x]
    | x :: y :: tail ->
      if compare x y = 0 then
        lp compare (x :: acc) tail
      else
        lp compare (y :: acc) tail
  in
  lp compare [] list

let dedup_right compare l =
  match l with
    [] -> []
  | [x] -> [x]
  | x :: y :: l ->
    if compare x y = 0 then
      x :: l
    else
      x :: y :: l

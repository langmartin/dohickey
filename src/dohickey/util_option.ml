let (>>=) = Option.bind
let (|>>) o f = (Option.map f) o

(** fold left while Some, stop at the first None *)
let rec fold_left_until f acc xs =
  match xs with
    [] -> acc
  | x :: xs -> match f acc x with
      None -> acc
    | Some acc -> fold_left_until f acc xs

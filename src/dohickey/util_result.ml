let (>>=) = Result.bind
let (|>>) r f = (Result.map f) r

(** fold left while Ok, stop at the first Error *)
let rec fold_left_until f acc xs =
  match xs with
  | [] -> Ok acc
  | x :: xs -> match f acc x with
    | Ok acc' -> fold_left_until f acc' xs
    | Error e -> Error e

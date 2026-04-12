open Option

let ( >>= ) = bind
let ( |>> ) o f = (map f) o

let product o1 o2 =
  match (o1, o2) with
  | (Some v1, Some v2) -> Some (v1, v2)
  | _ -> None

module Syntax = struct
  let ( let* ) = bind
  let ( let+ ) r f = map f r
  let ( and* ) = product
  let ( and+ ) = product
end

(** fold left while Some, stop at the first None *)
let rec fold_while f acc xs =
  match xs with
    [] -> acc
  | x :: xs -> match f acc x with
      None -> acc
    | Some acc -> fold_while f acc xs

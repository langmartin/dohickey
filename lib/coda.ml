type t = {time: string; user: string}

let empty = {time = ""; user = ""}

let compare a b =
  match String.compare a.time b.time with
  | 0 -> String.compare a.user b.user
  | c -> c

type body =
  | Text of {row: int; col: int; text: string}
  | Call of {id: string}
  | Stop of {id: string}
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
  | Stop item -> sprintf "stop-%s" item.id

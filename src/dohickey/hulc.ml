type serialized = string

type split = {
  time: string;
  node: string
}

type t = {
  time : Hlc.t;
  node : string
}

let sprint clock node_id =
  Hlc.sprint64 clock ^ node_id

let split_opt serialized : split option =
  match String.split_on_char '-' serialized with
  | time :: node :: [] -> Some { time; node }
  | _ -> None

let parse_opt serialized =
  match split_opt serialized with
  | Some { time; node } ->
    begin
      match Hlc.parse64_opt time with
      | Some time -> Some {time; node}
      | None -> None
    end
  | _ -> None

exception Argument

let split serialized =
  match split_opt serialized with
  | Some s -> s
  | None -> raise Argument

let parse serialized =
  match parse_opt serialized with
  | Some t -> t
  | None -> raise Argument

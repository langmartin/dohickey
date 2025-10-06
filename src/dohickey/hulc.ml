type serialized = string

type split = {
  time: string;
  node: string
}

type t = {
  time : Hlc.t;
  node : string
}

let init node =
  let open Hlc in
  let time = Hlc.init (time_ms()) in
  {time; node}

let send (local : t) =
  let open Hlc in
  let time = send (time_ms()) local.time in
  {local with time}

let recv (local : t) (remote : t) =
  let open Hlc in
  let time = recv (time_ms()) local.time remote.time in
  {local with time}

let sprint clock =
  let {time; node} = clock in
  Hlc.sprint64 time ^ node

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

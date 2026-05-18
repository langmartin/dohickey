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

let recv_safe system_time (local:t) (remote:t) =
  let open Hlc in
  match recv_safe system_time local.time remote.time with
    Error e -> Error e
  | Ok time -> Ok {local with time}

let sprint clock =
  let {time; node} = clock in
  Hlc.sprint64 time ^ node

let split_opt s : split option =
  let open String in
  if length s = 23 then
    let time = sub s 0 11 in
    let node = sub s 11 12 in
    Some {time; node}
  else
    None

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

let parse_safe serialized =
  match parse_opt serialized with
    None -> Error "imparseable"
  | Some t -> Ok t

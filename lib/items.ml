module Coda = struct
  type t = {time: string; user: string}
  let empty = {time = ""; user = ""}
  let compare a b = 
    match String.compare a.time b.time with
    | 0 -> String.compare a.user b.user
    | c -> c

  let of_json j =
    let open Yojson.Safe.Util in
    { time = j |> member "time" |> to_string;
      user = j |> member "user" |> to_string}
end

module Vote = struct
  type t = {row: int; col: int; id: string; rank: int}

  let of_json j =
    let open Yojson.Safe.Util in
    {row = j |> member "row" |> to_int;
     col = j |> member "col" |> to_int;
     id = j |> member "id" |> to_string;
     rank = j |> member "rank" |> to_int;}
end

module Item = struct
  type t =
    | Text of {coda: Coda.t; row: int; col: int; text: string}
    | Call of {coda: Coda.t; id: string}
    | Stop of {coda: Coda.t; id: string}
    | Vote of {coda: Coda.t; vote: Vote.t}
    | Result of {coda: Coda.t; vote: Vote.t}
    | None of {coda: Coda.t}

  let key item =
    let open Format in
    match item with
    | Text item -> sprintf "text-%d%d" item.row item.col
    | Vote item -> sprintf "vote-%d%d" item.vote.row item.vote.col
    | Result item -> sprintf "result-%d%d" item.vote.row item.vote.col
    | Call item -> sprintf "call-%s" item.id
    | Stop item -> sprintf "stop-%s" item.id
    | None _ -> ""

  exception Invalid_type

  let of_json j =
    let open Yojson.Safe.Util in
    match j |> member "type" |> to_string with
    | "text" -> Text {coda = Coda.of_json j;
                      row = j |> member "row" |> to_int;
                      col = j |> member "col" |> to_int;
                      text = j |> member "text" |> to_string}
    | "vote" -> Vote {coda = Coda.of_json j; vote = Vote.of_json j}
    | "call" -> Call {coda = Coda.of_json j; id = j |> member "id" |> to_string}
    | "stop" -> Stop {coda = Coda.of_json j; id = j |> member "id" |> to_string}
    | "result" -> Result {coda = Coda.of_json j; vote = Vote.of_json j}
    | _ -> raise Invalid_type
end

(* module CodaMap = Map.Make(Coda) *)
(* type items = CodaMap *)
(* let empty : Item.t CodaMap.t = CodaMap.empty *)

module StringMap = Map.Make(String)
type t = Item.t StringMap.t
let empty : t = StringMap.empty

let put item m =
  let open StringMap in
  let key = Item.key item in
  match find_opt key m with
  | None -> add key item m
  | Some prev ->
    if Coda.compare prev.coda item.coda < 0 then
      add key item m
    else
      m
      
let of_json j =
  let open Yojson.Safe.Util in
  j |> to_list |> List.map Item.of_json

(* Cleanup the names *)

let upper_ascii = "ABCDEFGHIJKLMNOPQRSTUVWXYZ"
let lower_ascii = "abcdefghijklmnopqrstuvwxyz"
let numeric = "0123456789"
let other_ascii = "_-"

let safe_char c =
  if String.contains upper_ascii c ||
     String.contains lower_ascii c ||
     String.contains numeric c ||
     String.contains other_ascii c
  then
    c
  else if c = ' ' then
    '-'
  else
    ' '

let dohickey_name input =
  input
  |> String.lowercase_ascii
  |> String.map safe_char
  |> String.split_on_char ' '
  |> List.filter (fun s -> String.length s <> 0)
  |> String.concat ""

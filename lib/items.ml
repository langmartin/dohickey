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

  let to_json c =
    `Assoc [("time", `String c.time); ("user", `String c.user)]
end

module Vote = struct
  type t = {row: int; col: int; id: string; rank: int}

  let of_json j =
    let open Yojson.Safe.Util in
    {row = j |> member "row" |> to_int;
     col = j |> member "col" |> to_int;
     id = j |> member "id" |> to_string;
     rank = j |> member "rank" |> to_int;}

  let to_json v =
    `Assoc [("row", `Int v.row); ("col", `Int v.col); ("id", `String v.id); ("rank", `Int v.rank)]
end

module Item = struct
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

  exception Invalid_type

  let of_json j =
    let open Yojson.Safe.Util in
    {
      coda = Coda.of_json j;
      body = match j |> member "type" |> to_string with
        | "text" -> Text {row = j |> member "row" |> to_int;
                          col = j |> member "col" |> to_int;
                          text = j |> member "text" |> to_string}
        | "vote" -> Vote (Vote.of_json j)
        | "call" -> Call {id = j |> member "id" |> to_string}
        | "stop" -> Stop {id = j |> member "id" |> to_string}
        | "result" -> Result (Vote.of_json j)
        | _ -> raise Invalid_type
    }

  let to_json i =
    let j = match i.body with
      | Text i -> `Assoc [("row", `Int i.row); ("col", `Int i.col); ("text", `String i.text)]
      | Vote i -> Vote.to_json i
      | Result i -> Vote.to_json i
      | Call i -> `Assoc [("id", `String i.id)]
      | Stop i -> `Assoc [("id", `String i.id)]
    in
    `Assoc [("coda", Coda.to_json i.coda); ("body", j)]
end

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

(** reduce [items] into map [m] returning the updated map and subset of the items that we fresh by [coda] and changed the state of [m] *)
let puts items m =
  List.fold_left (fun (m, l) item ->
      let m' = put item m in
      if m == m' then
        (m, l)
      else
        (m', item :: l))
    (m, [])
    items

exception EmptyList

let to_node_id (items : Item.t list) =
  match items with
  | [] -> raise EmptyList
  | item :: _ ->
    match String.split_on_char '-' item.coda.time with
    | _time :: node_id :: [] -> node_id
    | _ -> raise EmptyList

let of_json j =
  let open Yojson.Safe.Util in
  j |> to_list |> List.map Item.of_json

let to_json items =
  `List (List.map Item.to_json items)

let to_json_str items = Yojson.Basic.pretty_to_string (to_json items)

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

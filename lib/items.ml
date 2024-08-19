type coda = {time: string; user: string}

type vote = {row: int; col: int; id: string; rank: int}

type item =
  | Text of {coda: coda; row: int; col: int; text: string}
  | Call of {coda: coda; id: string}
  | Stop of {coda: coda; id: string}
  | Vote of {coda: coda; vote: vote}
  | Result of {coda: coda; vote: vote}
  | None

module Coda = struct
  type t = coda

  let compare a b =
    if a.time = b.time then
      0
    else if a.time < b.time then
      -1
    else
      1
end

module StringMap = Map.Make(String)
module CodaMap = Map.Make(Coda)

type item_history = CodaMap

let empty : item_history StringMap.t = StringMap.empty

(* Conversion from json *)

let coda_of_json j =
  let open Yojson.Basic.Util in
  { time = j |> member "time" |> to_string;
    user = j |> member "user" |> to_string}

let vote_of_json j =
  let open Yojson.Basic.Util in
  {row = j |> member "row" |> to_int;
   col = j |> member "col" |> to_int;
   id = j |> member "id" |> to_string;
   rank = j |> member "rank" |> to_int;}

let item_of_json j =
  let open Yojson.Basic.Util in
  match j |> member "type" |> to_string with
  | "text" -> Text {coda = coda_of_json j;
                    row = j |> member "row" |> to_int;
                    col = j |> member "col" |> to_int;
                    text = j |> member "text" |> to_string}
  | "vote" -> Vote {coda = coda_of_json j; vote = vote_of_json j}
  | "call" -> Call {coda = coda_of_json j; id = j |> member "id" |> to_string}
  | "stop" -> Stop {coda = coda_of_json j; id = j |> member "id" |> to_string}
  | "result" -> Result {coda = coda_of_json j; vote = vote_of_json j}
  | _ -> None

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

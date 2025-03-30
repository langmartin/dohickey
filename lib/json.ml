let coda_of_json j =
  let open Dohickey.Coda in
  let open Yojson.Safe.Util in
  { time = j |> member "time" |> to_string;
    user = j |> member "user" |> to_string }

let coda_to_json (c : Dohickey.Coda.t) =
  `Assoc [("time", `String c.time); ("user", `String c.user)]

let vote_of_json j =
  let open Dohickey.Vote in
  let open Yojson.Safe.Util in
  {row = j |> member "row" |> to_int;
   col = j |> member "col" |> to_int;
   id = j |> member "id" |> to_string;
   rank = j |> member "rank" |> to_int;}

let vote_to_json (v : Dohickey.Vote.t) =
  `Assoc [("row", `Int v.row);
          ("col", `Int v.col);
          ("id", `String v.id);
          ("rank", `Int v.rank)]

exception Invalid_type

let item_of_json j =
  let open Dohickey.Item in
  let open Yojson.Safe.Util in
  {
    coda = coda_of_json j;
    body = match j |> member "type" |> to_string with
      | "text" -> Text {row = j |> member "row" |> to_int;
                        col = j |> member "col" |> to_int;
                        text = j |> member "text" |> to_string}
      | "vote" -> Vote (vote_of_json j)
      | "call" -> Call {id = j |> member "id" |> to_string}
      | "stop" -> Stop {id = j |> member "id" |> to_string}
      | "result" -> Result (vote_of_json j)
      | _ -> raise Invalid_type
  }

let item_to_json (i : Dohickey.Item.t) =
  let j = match i.body with
    | Text i -> `Assoc [("row", `Int i.row); ("col", `Int i.col); ("text", `String i.text)]
    | Vote i -> vote_to_json i
    | Result i -> vote_to_json i
    | Call i -> `Assoc [("id", `String i.id)]
    | Stop i -> `Assoc [("id", `String i.id)]
  in
  `Assoc [("coda", coda_to_json i.coda); ("body", j)]

let of_json j =
  let open Yojson.Safe.Util in
  j |> to_list |> List.map item_of_json

let to_json items =
  `List (List.map item_to_json items)

let to_json_str items =
  items |> to_json |> Yojson.Basic.pretty_to_string

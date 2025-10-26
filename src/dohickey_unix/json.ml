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
  let jc = member "coda" j in
  let jb = member "body" j in
  {
    coda = coda_of_json jc;
    body = match j |> member "type" |> to_string with
      | "text" -> Text {row = jb |> member "row" |> to_int;
                        col = jb |> member "col" |> to_int;
                        text = jb |> member "text" |> to_string}
      | "vote" -> Vote (vote_of_json jb)
      | "call" -> Call {id = jb |> member "id" |> to_string}
      | "count" -> Count {id = jb |> member "id" |> to_string}
      | "result" -> Result (vote_of_json jb)
      | "title" -> Title (to_string jb)
      | _ -> raise Invalid_type
  }

let item_to_json (i : Dohickey.Item.t) =
  let (t, j) = match i.body with
    | Text i -> "text",
                `Assoc [("row", `Int i.row);
                        ("col", `Int i.col);
                        ("text", `String i.text)]
    | Vote i -> "vote", vote_to_json i
    | Result i -> "result", vote_to_json i
    | Call i -> "call", `Assoc [("id", `String i.id)]
    | Count i -> "count", `Assoc [("id", `String i.id)]
    | Title i -> "title", `String i
  in
  `Assoc [("coda", coda_to_json i.coda); ("body", j); ("type", `String t)]

let of_json j =
  let open Yojson.Safe.Util in
  j |> to_list |> List.map item_of_json

let of_list_json jss =
  List.map item_of_json jss

let of_json_str s =
  s |> Yojson.Safe.from_string |> of_json

let to_json items =
  `List (List.map item_to_json items)

let to_json_str items =
  items |> to_json |> Yojson.Basic.to_string

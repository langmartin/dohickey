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
   rank = j |> member "rank" |> to_int;}

let vote_to_json (v : Dohickey.Vote.t) =
  `Assoc [("row", `Int v.row);
          ("col", `Int v.col);
          ("rank", `Int v.rank)]

exception Invalid_type

let item_of_json j =
  let open Dohickey.Item in
  let open Yojson.Safe.Util in
  let jc = member "coda" j in
  let jb = member "body" j in
  let ok body = Some { coda = coda_of_json jc; body } in
  match j |> member "type" |> to_string with
  | "text" -> Text {row = jb |> member "row" |> to_int;
                    col = jb |> member "col" |> to_int;
                    text = jb |> member "text" |> to_string} |> ok
  | "vote" -> Vote (vote_of_json jb) |> ok
  | "count" -> Count true |> ok
  | "result" -> Result (vote_of_json jb) |> ok
  | "title" -> Title (to_string jb) |> ok
  | _ -> None

let item_to_json (i : Dohickey.Item.t) =
  let (t, j) = match i.body with
    | Text i -> "text",
                `Assoc [("row", `Int i.row);
                        ("col", `Int i.col);
                        ("text", `String i.text)]
    | Vote i -> "vote", vote_to_json i
    | Result i -> "result", vote_to_json i
    | Count _ -> "count", `Bool true
    | Title i -> "title", `String i
    | Error i -> "error", `String i
  in
  `Assoc [("coda", coda_to_json i.coda); ("body", j); ("type", `String t)]

let of_json json_str =
  let open Yojson.Safe.Util in
  json_str
  |> to_list
  |> List.map item_of_json
  |> List.concat_map Option.to_list

let of_list_json jss =
  jss
  |> List.map item_of_json
  |> List.concat_map Option.to_list

let of_json_str s =
  s |> Yojson.Safe.from_string |> of_json

let to_json items =
  `List (List.map item_to_json items)

let to_json_str items =
  items |> to_json |> Yojson.Basic.to_string

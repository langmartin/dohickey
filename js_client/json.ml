open Brr
open Brr_io

let coda_of_json j =
  let open Dohickey.Coda in
  let open Jv in
  { time = get j "time" |> to_string;
    user = get j "user" |> to_string; }

let coda_to_json (c : Dohickey.Coda.t) =
  Jv.obj [|
    ("time", (Jv.of_string c.time));
    ("user", (Jv.of_string c.user))
  |]

let vote_of_json j =
  let open Dohickey.Vote in
  let open Jv in
  { row = get j "row" |> to_int;
    col = get j "col" |> to_int;
    id =  get j "id" |> to_string;
    rank = get j "rank" |> to_int; }

let vote_to_json (v : Dohickey.Vote.t) =
  let open Jv in
  obj [|
    ("row", of_int v.row);
    ("col", of_int v.col);
    ("id", of_string v.id);
    ("rank", of_int v.rank)
  |]

open Brr
open Util

let _id ids =
  ids
  |> List.map Int.to_string
  |> List.cons "dh"
  |> String.concat "-"

let vote_btn send_vote dir =
  El.button [El.txt' dir]
  |> add_ev_listener Ev.click send_vote

(* TODO: on_click event handler that sends a vote *)
let vote_ctx vote row col =
  let data_row i = At.(int (Jstr.v "data-row") i) in
  let data_col i = At.(int (Jstr.v "data-col") i) in
  let vote_btn = vote_btn vote in
  El.div ~at:[
    At.hidden;
    cls ["ballot-box"];
    data_row row;
    data_col col
  ]
    [vote_btn "+"; vote_btn "-"]

let td_txt row col =
  let content text =
    El.div ~at:[cls ["content"]]
      [El.txt' text]
  in
  (if row = 0 then
     "Option #" ^ (Int.to_string col)
   else if col = 0 then
     "Goal #" ^ (Int.to_string row)
   else
     "deets")
  |> content

let th id row col =
  El.th ~at:[id' id]
    [td_txt row col]

let td ~vote id row col =
  El.td ~at:[id' id]
    [td_txt row col; vote_ctx vote row col]

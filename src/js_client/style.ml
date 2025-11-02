open Brr
open Util

let vote_btn send_vote dir =
  El.button [El.txt' dir]
  |> add_ev_listener Ev.click send_vote

(* TODO: on_click event handler that sends a vote *)
let vote_ctx vote row col =
  let vote_btn = vote_btn vote in
  El.div ~at:[
    At.hidden;
    cls ["ballot-box"];
    At.(int (Jstr.v "data-row") row);
    At.(int (Jstr.v "data-col") col)
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

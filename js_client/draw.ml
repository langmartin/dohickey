(*

   The service worker receives incoming messages and in turn sends
   them as messages to us, the client. These receivers draw the
   updates.

*)

open Brr
open Util

let rows() = qsa "#dohickey tr"
let cols() = qsa "#dohickey tr:first-child td"

(*
   Find the event context
*)

let find_td = find_parent "td"

(*
   Voting.
*)

let at_id el =
  match El.at (Jstr.v "id") el with
  | None -> None
  | Some id -> Some (Jstr.to_string id)

let btn_rank el =
  let txt = El.text_content el |> Jstr.to_string in
  if txt = "+" then 1 else -1

let at_vote btn el =
  let row = at_int "data-row" el in
  let col = at_int "data-col" el in
  let id = at_str "data-call" el in
  let rank = btn_rank btn in
  let open Dohickey.Vote in
  {row; col; id; rank}

let send_vote ev =
  let btn = event_el ev in
  match El.parent btn with
  | None -> ()
  | Some el ->
    at_vote btn el
    |> Send.vote

let vote_btn dir =
  El.button [El.txt' dir]
  |> add_ev_listener Ev.click send_vote

(* TODO: on_click event handler that sends a vote *)
let vote_ctx row col =
  let el = El.div ~at:[At.hidden]
      [vote_btn "+"; vote_btn "-"] in
  [("voting", false); ("ballot-box", true)]
  |> set_classes el;
  [("data-row", Int row); ("data-col", Int col)]
  |> set_attrs el;
  el

let call_one_vote id el =
  set_classes el [("voting", true)];
  set_attrs el [("data-call", Str id)]

let end_one_vote el =
  set_classes el [("voting", false)];
  set_attrs el [("data-call", Gone)]

let each f lst =
  List.fold_left (fun _ x -> f x) () lst

let call_vote id = qsa "#dohickey .ballot-box" |> each (call_one_vote id)
let end_vote = qsa "#dohickey .ballot-box" |> each end_one_vote

(*
   Text
*)

let send_text e =
  let el = event_el e in
  let text = el |> El.text_content |> Jstr.to_string in
  (* Like piping each step with |> Option.bind but using fancy syntax *)
  let (>>=) = Option.bind in
  find_td el
  >>= at_id
  >>= Dohickey.Item.parse_pos "text"
  >>= (fun body -> Some (Send.text {body with text=text}))
  |> ignore

let editable txt =
  El.textarea
    [El.txt txt]
  |> add_ev_listener Ev.focusout send_text

let lemme_edit e =
  let el = Ev.target e |> Ev.target_to_jv |> El.of_jv in
  match El.find_first_by_selector ~root:el (Jstr.v ".content") with
  | None -> ()
  | Some ct ->
    let txt = El.text_content ct in
    El.set_children ct
      [editable txt]

(*
   TODO: contenteditable(?) and event handler to send text.
   Also send "typing..."
*)
let dh_td row col =
  let id = Dohickey.Item.key_text row col in
  El.td ~at:[(At.id (Jstr.v id))]
    [vote_ctx row col;
     El.div ~at:[(At.class' (Jstr.v "content"))]
       [El.txt' ""]]
  |> add_ev_listener Ev.click lemme_edit

(*
   Table
*)

let sync_cols n =
  let open List in
  rows()
  |> mapi (fun row tr ->
      repeatedly
        (fun col ->
           [dh_td row col]
           |> El.append_children tr)
        n)

let row_el ncols row =
  let elf = dh_td row in
  let tds = repeatedly elf ncols in
  El.tr tds

let sync_rows n ncols =
  match qsa "#dohickey tbody" with
  | [tb] -> repeatedly (row_el ncols) n |> El.append_children tb
  | _ -> ()

let item_text (body : Dohickey.Item.text_body) =
  let open Dohickey.Item in
  let id = key_text body.row body.col in
  let els = ["#"; id; " .content"] |> String.concat "" |> qsa in
  match els with
  | [el] ->
    let el = El.to_jv el in
    ignore @@ Jv.call el "textContent" [| (Jv.of_string body.text) |]
  | _ -> ()

let item_call (body : Dohickey.Item.call_body) = call_vote body.id
let item_count () = end_vote

(*
   ======================================================================
   Received event handlers
*)

let dims (row, col) =
  let rn = rows() |> List.length in
  let cn = cols() |> List.length in
  sync_rows (row - rn) col;
  ignore @@ sync_cols (col - cn)

let item (item : Dohickey.Item.t) =
  match item.body with
  | Text it -> item_text it
  | Call it -> item_call it
  | Count _it -> item_count() (* TODO match the id? Maybe server already did *)
  | Vote _it -> ()
  | Result _it -> ()

(*

   The service worker receives incoming messages and in turn sends
   them as messages to us, the client. These receivers draw the
   updates.

*)

open Brr
open Util

(*
   Find the event context
*)

let find_td = find_parent "td"

(*
   Voting.
*)

let el_id el =
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
  let data_row i = At.(int (Jstr.v "data-row") i) in
  let data_col i = At.(int (Jstr.v "data-col") i) in
  El.div ~at:[
    At.hidden;
    cls ["ballot-box"];
    data_row row;
    data_col col
  ]
    [vote_btn "+"; vote_btn "-"]

let call_one_vote id el =
  set_classes el [("voting", true)];
  set_attrs el [("data-call", Str id)]

let end_one_vote el =
  set_classes el [("voting", false)];
  set_attrs el [("data-call", Gone)]

let call_vote id = qsa "#dohickey .ballot-box" |> each (call_one_vote id)
let end_vote() = qsa "#dohickey .ballot-box" |> each end_one_vote

(*
   Text
*)

let send_text e =
  let el = event_el e in
  let text = el |> El.text_content |> Jstr.to_string in
  (* Like piping each step with |> Option.bind but using fancy syntax *)
  find_td el
  >>= el_id
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
   Table
*)

let is_header row col = row = 0 || col = 0

let idstr ids =
  ids
  |> List.map Int.to_string
  |> String.concat "-"

let find_el id =
  let qs = ["#"; id] |> String.concat "" in
  match qsa qs with
  | [el] -> Some el
  | _ -> None

let get_row row =
  let id = idstr [row] in
  match find_el id with
  | Some el -> el
  | None -> El.tr ~at:[id' id] []

(*
   TODO: contenteditable(?) and event handler to send text.
   Also send "typing..."

   The only difference between headers and bodies is whether they
   contain a vote_ctx, so that's an option.
*)
let make_txt () =
  El.div ~at:[cls ["content"]]
    [El.txt' ""]

let make_th id =
  El.th ~at:[id' id]
    [make_txt()]

let make_td id row col =
  El.td ~at:[id' id]
    [make_txt(); vote_ctx row col]

let make_cell row col =
  let id = idstr [row; col] in
  (if is_header row col then
     make_th id
   else
     make_td id row col)
  |> add_ev_listener Ev.click lemme_edit

let sync_td parent row col =
  let id = idstr [row; col] in
  match find_el id with
  | Some _el -> ()
  | None -> [make_cell row col] |> El.append_children parent

let sync_cols ncols (row : int) =
  (* 0 is for the headers, so we want one extra iteration *)
  let el = get_row row in
  for col = 0 to ncols do
    sync_td el row col
  done

let sync_rows n ncols =
  for row = 0 to n do
    sync_cols ncols row
  done

let item_text (body : Dohickey.Item.text_body) =
  let open Dohickey.Item in
  let id = idstr [body.row; body.col] in
  let els = ["#"; id; " .content"] |> String.concat "" |> qs1 in
  match els with
  | Some el ->
    let el = El.to_jv el in
    ignore @@ Jv.call el "textContent" [| (Jv.of_string body.text) |]
  | None -> ()

let item_call (body : Dohickey.Item.call_body) = call_vote body.id
let item_count () = end_vote()

(*
   ======================================================================
   Received event handlers
*)

let dims (row, col) =
  ignore @@ sync_rows row col

let item (item : Dohickey.Item.t) =
  match item.body with
  | Text it -> item_text it
  | Call it -> item_call it
  | Count _it -> item_count() (* TODO match the id? Maybe server already did *)
  | Vote _it -> ()
  | Result _it -> ()

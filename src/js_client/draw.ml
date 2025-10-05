(*

   The service worker receives incoming messages and in turn sends
   them as messages to us, the client. These receivers draw the
   updates.

*)

open Brr
open Util

let make_id ids =
  ids
  |> List.map Int.to_string
  |> List.cons "dh"
  |> String.concat "-"

let parse_id ids =
  Dohickey.Item.parse_pos "dh" ids

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

let flag = "data-editing"

let find_cell ev =
  let trg = event_el ev in
  match find_parent (is_tag ["th"; "td"]) trg with
  | Some el -> el
  | None -> trg

let el_text el = Some (El.text_content el)
let el_value el = Some (El.prop El.Prop.value el)

let to_s jst = match jst with
  | Some jst -> Jstr.to_string jst
  | None -> ""

let content_text el =
  qs1 ~el:el ".content" >>= el_text |> to_s

let editor_text el =
  qs1 ~el:el ".editor input" >>= el_value |> to_s

let el_remove el = Some (El.remove el)

let send_text text body =
  Some (Send.text {body with text=text})

let send_text e =
  Ev.stop_propagation e;
  let el = find_cell e in
  let text = editor_text el in
  (* Build and send the message *)
  el_id el
  >>= parse_id
  >>= send_text text
  |> ignore;

  (* Remove the flag that prevents double editing *)
  set_attrs el [(flag, Gone)];

  (* Remove the editor *)
  qs1 ~el:el ".editor" >>= el_remove |> ignore

let editable txt =
  El.div ~at:[cls ["editor"]]
    [El.input ~at:[At.type' (Jstr.v "text"); At.placeholder (Jstr.v txt)] ();
     El.button
       [El.txt' "send"]
     |> add_ev_listener Ev.click send_text]

let lemme_edit e =
  Ev.stop_propagation e;
  let el = find_cell e in
  if at_bool flag el then
    ()
  else
    let text = content_text el in
    set_attrs el [(flag, True)];
    El.append_children el
      [editable text]

(*
   Table
*)

let is_header row col = row = 0 || col = 0

let find_el id =
  let qs = ["#"; id] |> String.concat "" in
  match qsa qs with
  | [el] -> Some el
  | _ -> None

let append_row row =
  match qs1 "#dohickey tbody" with
  | None -> ()
  | Some el -> El.append_children el [row]

let get_row row =
  let id = make_id [row] in
  match find_el id with
  | Some el -> el
  | None ->
    let row = El.tr ~at:[id' id] [] in
    append_row row;
    row

(*
   TODO: contenteditable(?) and event handler to send text.
   Also send "typing..."

   The only difference between headers and bodies is whether they
   contain a vote_ctx, so that's an option.
*)
let make_cell row col =
  let id = make_id [row; col] in
  (if is_header row col then
     Style.th id row col
   else
     Style.td ~vote:send_vote id row col)
  |> add_ev_listener Ev.click lemme_edit

let sync_td parent row col =
  let id = make_id [row; col] in
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

open Dohickey

let item_text (body : Item.text_body) (coda : Coda.t) =
  let open Dohickey.Item in
  let _user = coda.user in
  let id = make_id [body.row; body.col] in
  let els = ["#"; id; " .content"] |> String.concat "" |> qs1 in
  match els with
  | Some el ->
    El.set_children el
      [El.txt' body.text]
  | None -> ()

let item_call (body : Item.call_body) = call_vote body.id
let item_count () = end_vote()

(*
   ======================================================================
   Received event handlers
*)

let dims (row, col) =
  ignore @@ sync_rows row col

let item (item : Item.t) =
  match item.body with
  | Text it -> item_text it item.coda
  | Call it -> item_call it
  | Count _it -> item_count() (* TODO match the id? Maybe server already did *)
  | Vote _it -> ()
  | Result _it -> ()

let title title =
  match qs1 "#title" with
  | Some el -> El.set_children el [El.txt' title]
  | None -> ()

let user username =
  match qs1 "#user" with
  | Some el -> El.set_children el [El.txt' username]
  | None -> ()

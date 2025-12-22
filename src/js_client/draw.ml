(*

   The service worker receives incoming messages and in turn sends
   them as messages to us, the client. These receivers draw the
   updates.

*)

open Brr
open Util

(*
   Text
*)

let el_value el = Some (El.prop El.Prop.value el)

let to_s jst = match jst with
  | Some jst -> Jstr.to_string jst
  | None -> ""

let main_text el =
  qs1 ~el:el "[name=main]"
  >>= el_value
  |> to_s

let send_title ev =
  Ev.stop_propagation ev;
  Ev.prevent_default ev;
  ev |> event_el |> main_text |> Send.title

let after f g = fun ev -> f ev; g(); ()

let main_attr txt =
  [At.placeholder (Jstr.v txt);
   At.name (Jstr.v "main")]

let editable_title undo txt =
  El.form ~at:[cls ["editor"]]
    [El.textarea ~at:(main_attr txt)
       [El.txt' txt];
     El.button ~at:[At.type' (Jstr.v "submit")]
       [El.txt' "send"]]
  |> el_on_submit (after send_title undo)

(*
   Table
*)

let append_row row =
  match qs1 "#dohickey tbody" with
  | None -> ()
  | Some el -> El.append_children el [row]

let get_row row =
  let id = Draw_common.row_id row in
  match qs1 id.qs with
  | Some el -> el
  | None ->
    let row = El.tr ~at:[id' id.id] [] in
    append_row row;
    row

let sync_td parent row col =
  let id = Draw_common.cell_id row col in
  match qs1 id.qs with
  | Some _el -> ()
  | None -> [Draw_text.make_cell row col] |> El.append_children parent

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

let item_title title =
  match qs1 "#title" with
  | Some el -> El.set_children el [El.txt' title]
  | None -> ()

(*
   ======================================================================
   Received event handlers
*)

let dims (row, col) =
  ignore @@ sync_rows row col

let item (item : Item.t) =
  match item.body with
  | Text it -> Draw_text.item_text it item.coda
  | Count _ -> ()
  | Vote it -> Draw_vote.vote it
  | Result _it -> ()
  | Title it -> item_title it

let user username =
  match qs1 "#user" with
  | Some el -> El.set_children el [El.txt' username]
  | None -> ()

let hist (item : Item.t) =
  Draw_history.item item

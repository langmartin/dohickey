(*

   The service worker receives incoming messages and in turn sends
   them as messages to us, the client. These receivers draw the
   updates.

*)

open Brr
open Util
open Dohickey.Util_option

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

(*
   ======================================================================
   Track the set of users for cursor display
*)

let add_user user =
  let open Store in
  if not (S.mem user cursor.users) then
    cursor.users <- S.add user cursor.users

let user_cls user =
  let open Store in
  if cursor.user = user then "cursor-self" else
    let io = S.to_list cursor.users |> List.find_index (( = ) user) in
    match io with
    | None -> "cursor-user-4"
    | Some i -> "cursor-user-" ^ (Int.to_string (i mod 4))

(*
   ======================================================================
   Display page values & cursors
*)

open Dohickey

let item_title title =
  match qs1 "#title" with
  | Some el -> El.set_children el [El.txt' title]
  | None -> ()

let mv_cls cls cell_qs =
  let set is_set el = El.set_class (Jstr.v cls) is_set el in
  "." ^ cls |> qs1 |>> set false |> ignore;
  cell_qs |> qs1 |>> set true |> ignore

let set_cursor user row col =
  if Store.cursor.user = user then
    Store.cursor.pos <- row, col;
  let cell = Draw_common.cell_id row col in
  mv_cls (user_cls user) cell.qs

(*
   ======================================================================
   Received event handlers
*)

let dims (row, col) =
  Console.debug [row; col];
  ignore @@ sync_rows row col

let item (item : Item.t) =
  match item.body with
  | Text it -> Draw_text.item_text it item.coda
  | Count _ -> ()
  | Vote it -> Draw_vote.vote it
  | Result _it -> ()
  | Title it -> item_title it
  | Cursor c ->
    add_user item.coda.user;
    set_cursor item.coda.user c.row c.col
  | Error it ->
    (* FIXME display errors *)
    Console.error [it]

let user username =
  Store.set_self username;
  match qs1 "#user" with
  | Some el -> El.set_children el [El.txt' username]
  | None -> ()

let hist (item : Item.t) =
  Draw_history.item item

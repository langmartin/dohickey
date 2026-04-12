open Brr
open Dohickey
open Util

let set_div_text el txt = El.set_children el [El.txt' txt]
let txt_content el = El.text_content el |> Jstr.to_string |> String.trim

let set_content row col =
  ignore @@
  let open Dohickey.Util_option.Syntax in
  let* editor = qs1 "#cli-cli" in
  let src_id = Draw_common.cell_id row col in
  let* src = qs1 (src_id.qs ^ " .text") in
  let txt = txt_content src in
  set_classes editor [("placeholder", false)];
  set_div_text editor txt;
  El.set_has_focus true editor;
  Some ()

let clear_content() =
  ignore @@
  let open Dohickey.Util_option.Syntax in
  let* editor = qs1 "#cli-cli" in
  set_classes editor [("placeholder", true)];
  set_div_text editor "lorem ipsum";
  El.set_has_focus false editor;
  Some ()

let on_keydown ev =
  Ev.stop_propagation ev

let on_input ev =
  Ev.prevent_default ev;
  Ev.stop_propagation ev;
  let el = ev_target_el ev in
  let txt = txt_content el in
  Store.input_buf := txt

let on_save ev =
  Ev.prevent_default ev;
  Ev.stop_propagation ev;
  let (row, col) = Store.cursor.pos in
  let text = ! Store.input_buf in
  Store.cursor.editing <- false;
  Store.input_buf := "";
  Send.text Text.{row; col; text};
  clear_content()

let start() =
  let (row, col) = Store.cursor.pos in
  Store.cursor.editing <- true;
  set_content row col

let init() =
  ignore @@
  let open Dohickey.Util_option.Syntax in
  let* editor = qs1 "#cli-cli[data-inert]" in
  let* save = qs1 "#cli-save[data-inert]" in
  editor
  |> add_ev_listener Ev.keydown on_keydown
  |> add_ev_listener Ev.input on_input
  |> ignore;

  save
  |> add_ev_listener Ev.click on_save
  |> ignore;
  Some ()

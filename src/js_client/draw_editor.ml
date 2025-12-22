open Brr
open Dohickey
open Dohickey.Text
open Util

module StringMap = Map.Make(String)
type edit = Text.t StringMap.t

type state = {
  mutable selected : string option;
  mutable editing : edit
}

let state = {
  selected = None;
  editing = StringMap.empty
}

let editor_qs = "#editor [name=text]"

let set_and_focus_text text focus =
  match qs1 editor_qs with None -> () | Some el ->
    let jv = El.to_jv el in
    Jv.set jv "value" (Jv.of_string text);
    if focus then El.set_has_focus focus el else ()

let set_text text = set_and_focus_text text true
let clr_text() = set_and_focus_text "" false

let set_editing body =
  let {row; col; _} = body in
  let id = Draw_common.cell_id row col in
  state.editing <- StringMap.add id.id body state.editing;
  state.selected <- Some id.id

let get_editing id =
  StringMap.find id state.editing

let with_input body =
  match qs1 editor_qs with None -> body | Some el ->
    let jv = El.to_jv el in
    let text = Jv.get jv "value" |> Jv.to_string in
    {body with text}

let stop_editing () =
  match state.selected with None -> () | Some id ->
    state.editing <- StringMap.remove id state.editing;
    state.selected <- None;
    clr_text()

let edit_cell row col =
  let id = Draw_common.cell_id row col in
  let qs = id.qs ^ " .cell .text" in
  (* TODO: use any existing value that's in editing instead. *)
  match qs1 qs with None -> () | Some el ->
    let text = El.text_content el |> Jstr.to_string |> String.trim in
    let body = Text.{row; col; text} in
    set_editing body;
    set_text text

let on_submit ev =
  Ev.prevent_default ev;
  Ev.stop_propagation ev;
  match state.selected with None -> () | Some id ->
    get_editing id |> with_input |> Send.text;
    stop_editing()

let init () =
  Util.on_submit "#editor" on_submit

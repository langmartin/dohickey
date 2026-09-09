open Brr
open Util

let help = [
  "edit A1";
  "vote A1 1-5";
]

let input_help text =
  let s = String.edit_distance text in
  let max' a b = if (s a) < (s b) then a else b in
  help
  |> List.fold_left max' ""

let set_help_text text =
  match qs1 "#cli-help" with None -> () | Some el ->
    El.set_children el [El.txt' text]

let input_event ev =
  Ev.stop_propagation ev;
  Ev.prevent_default ev;
  ev |> ev_target_el |> El.text_content |> Jstr.to_string |> input_help |> set_help_text

type modifier = None | Alt | Ctrl
type command = Left | Right | Up | Down | Edit | Vote

let menu = [
  None, "ArrowLeft", Left;
  None, "ArrowRight", Right;
  None, "ArrowUp", Up;
  None, "ArrowDown", Down;

  None, "h", Left;
  None, "l", Right;
  None, "k", Up;
  None, "j", Down;

  None, "Enter", Edit;

  Ctrl, "f", Left;
  Ctrl, "b", Right;
  Ctrl, "p", Up;
  Ctrl, "n", Down;

  Alt, "e", Edit;
  Alt, "v", Vote;
]

let show qs show =
  (* get from cursor.pos *)
  let cls on off =
    match qs1 qs with None -> () | Some el ->
      El.set_class (Jstr.v off) false el;
      El.set_class (Jstr.v on) true el
  in
  if show then
    cls "open" "closed"
  else
    cls "closed" "open"

let show_vote() = show "#cli-cli" false; show "#cli-vote-opts" true
let show_cli() =  show "#cli-cli" true;  show "#cli-vote-opts" false

let send c =
  let open Store in
  match c with
  | Left | Right | Up | Down -> Send.cursor cursor.pos
  | Edit -> show_cli(); Cli_edit.start()
  | Vote -> show_vote(); Send.cli_vote cursor.pos

let move c =
  let open Store in
  let (row, col) = cursor.pos in
  let (mrow, mcol) = cursor.edge in
  (* allow the cursor to move to max row + 1 for is_add *)
  let (nr, nc) = (mrow + 1, mcol + 1) in
  let pos = match c with
    | Left -> row, col - 1 |> max 0
    | Right -> row, col + 1 |> min nc
    | Up -> row - 1 |> max 0, col
    | Down -> row + 1 |> min nr, col
    | _ -> cursor.pos
  in
  let is_add = match c with
    | Right -> col = mcol
    | Down -> row = mrow
    | _ -> false
  in
  if is_add then Draw.dims pos;
  cursor.pos <- pos;
  send c

let is_modifier_match ev m =
  let open Ev.Keyboard in
  match m with
  | None -> not (alt_key ev || ctrl_key ev || meta_key ev)
  | Ctrl -> ctrl_key ev
  | Alt -> meta_key ev || alt_key ev

let menu_select ev =
  ignore @@
  let open Dohickey.Util_option.Syntax in
  let kev = Ev.as_type ev in
  let key = kev |> Ev.Keyboard.key |> Jstr.to_string in
  let* (modifier, _, cmd) = List.find_opt (fun (_m, k, _c) -> key == k) menu in
  if is_modifier_match kev modifier then
    begin
      Ev.stop_propagation ev;
      Ev.prevent_default ev;
      move cmd;
      Some ()
    end
  else None

let attach_cli_handler() =
  ignore @@
  let open Dohickey.Util_option.Syntax in
  let* body = qs1 "body" in
  let* cli = qs1 "#cli[data-inert]" in
    set_classes cli ["data-inert", false];
    body
    |> add_ev_listener Ev.keydown menu_select
    |> El.set_has_focus true
    |> ignore;
    Some ()

(* Handlers for main *)

let init () =
  attach_cli_handler()

let dims row col =
  let open Store in
  let (pr, pc) = cursor.edge in
  cursor.edge <- max row pr, max col pc

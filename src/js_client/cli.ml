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
  Console.debug [text];
  match qs1 "#cli-help" with None -> () | Some el ->
    El.set_children el [El.txt' text]

let input_event ev =
  Ev.stop_propagation ev;
  Ev.prevent_default ev;
  ev |> ev_target_el |> El.text_content |> Jstr.to_string |> input_help |> set_help_text

let attach_cli_handler () =
  match qs1 "#cli-cli[data-inert]" with None -> () | Some el ->
    set_classes el ["data-inert", false];
    ignore @@ add_ev_listener Ev.input input_event el

let init () =
  attach_cli_handler()

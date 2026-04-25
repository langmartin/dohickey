open Brr
open Util

let input_help _text =
  "help output"

let set_help_text _text =
  ()

let input_event ev =
  Ev.stop_propagation ev;
  Ev.prevent_default ev;
  ev |> ev_target_el |> El.text_content |> input_help |> set_help_text

let attach_cli_handler () =
  qs1 "#cli-cli[data-inert]"
  |>> (fun el ->
      add_ev_listener Ev.input input_event el
    )

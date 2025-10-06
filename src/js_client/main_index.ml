open Brr
open Util

let load_tables() =
  ()

let create_table _name =
  ()

let create_table_on ev =
  Ev.stop_propagation ev;
  Ev.prevent_default ev;
  let form = event_el ev in
  qs1 ~el:form "[name=name]"
  |>> el_value_content
  |>> create_table
  |> ignore

let main() =
  on_submit "#table-create" create_table_on

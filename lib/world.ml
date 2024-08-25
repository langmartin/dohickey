let the_store = ref Tables.empty
let the_clients : (int, Dream.websocket) Hashtbl.t = Hashtbl.create 20

let broadcast items =
  the_clients
  |> Hashtbl.to_seq_values
  |> List.of_seq
  |> Lwt_list.iter_p
    (fun client ->
      Dream.send client (Items.to_json_str items))

let add_client websocket =
  let id = (Hashtbl.length the_clients) + 1 in
  Hashtbl.replace the_clients id websocket;
  id

let stop_client id =
  Hashtbl.remove the_clients id

let puts table items =
  let (store', items') = Tables.puts table items !the_store in
  the_store := store';
  (* maybe I've got an lwt promise to unit at some point rather than a unit? *)
  broadcast items'

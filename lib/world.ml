let the_store = ref Tables.empty
let the_clients : (int, Dream.websocket) Hashtbl.t = Hashtbl.create 20

let broadcast _items =
  the_clients
  |> Hashtbl.to_seq_values
  |> List.of_seq
  |> Lwt_list.iter_p (fun client -> Dream.send client "")

let add_client websocket =
  let id = (Hashtbl.length the_clients) + 1 in
  Hashtbl.replace the_clients id websocket;
  id

let stop_client id =
  Hashtbl.remove the_clients id

let puts table items =
  let (store', items') = Tables.puts table items !the_store in
  the_store := store';
  let _ = broadcast items' in
  ()

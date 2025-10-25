open Indexeddb
open Js_common
open Lwt.Syntax

let db_version = 4
let db_name = Jstr.of_string "dohickey"

(* See https://github.com/openEngiadina/geopub/blob/main/src/geopub/database/store.ml
   for an example of the author using their bindings *)

type queued =
  | Add of Jv.t * Dohickey.Item.t
  | Delete of Dohickey.Item.t

type state = {
  queue : queued Queue.t;
  mutable running : bool;
  mutable ready : bool
}

let state = {
  queue = Queue.create();
  running = false;
  ready = false;
}

let is_os db name =
  Database.object_store_names db
  |> List.find_opt (( = ) name)
  |> Option.is_some

let on_version_change db =
  if is_os db db_name then
    Database.delete_object_store db db_name;
  let os = Database.create_object_store db db_name in
  let name = Jstr.v "table" in
  let key = Jv.of_string "table" in
  let opts = Jv.obj [| ("unique", Jv.of_bool false); |] in
  ObjectStore.create_index os ~key_path:key ~object_parameters:opts name
  |> ignore

let open_db() =
  let* db = Database.open'
      ~version:db_version
      ~on_version_change:on_version_change
      db_name
  in
  state.ready <- true;
  Lwt.return db

let load_table table =
  let* db = open_db() in
  let txn = Transaction.create db ~mode:ReadOnly [db_name] in
  let os = Transaction.object_store txn db_name in
  let idx = ObjectStore.index os (Jstr.v table) in
  let key = KeyRange.only (Jv.of_string table) |> KeyRange.to_jv in
  let* xs = Index.get_all idx key in
  xs
  |> List.map Jv_item.obj_to_item
  |> List.concat_map Option.to_list
  |> Lwt.return

let dequeue() = Queue.take_opt state.queue

let save_one_item os (table, item) =
  let open Dohickey.Item in
  let key = item.coda.time |> Jv.of_string in
  let item = Jv_item.of_item item in
  Jv.set item "table" table;

  Brr.Console.debug ["SAVE", item];

  let* _ = ObjectStore.put os ~key item in
  Lwt.return_unit

let delete_one_item os item =
  let open Dohickey.Item in
  let key = item.coda.time |> Jv.of_string in
  let* _ = ObjectStore.delete os key in
  Lwt.return_unit

let rec drain os =
  match dequeue() with
  | None -> Lwt.return_unit
  | Some (Add (table, item)) ->
    let* _ = save_one_item os (table, item) in
    drain os
  | Some (Delete item) ->
    let* _ = delete_one_item os item in
    drain os

let start_saving() =
  if state.running || not state.ready then
    Brr.Console.debug ["START"; state.running; state.ready]
  else
    ignore @@
    begin
      state.running <- true;
      let* db = open_db() in
      let open Transaction in
      let txn = create db ~mode:ReadWrite [db_name] in
      let os = object_store txn db_name in
      let* _ = drain os in
      commit txn;
      state.running <- false;
      Lwt.return_unit
    end

let save_item table item =
  let table = Jv.of_string table in
  Queue.add (Add (table, item)) state.queue;
  start_saving()

let delete_item item =
  Queue.add (Delete item) state.queue;
  start_saving()

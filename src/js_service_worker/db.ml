open Indexeddb
open Js_common
open Lwt.Syntax

let db_version = 1
let db_name = Jstr.of_string "dohickey"

let (>>=) = Option.bind

(* See https://github.com/openEngiadina/geopub/blob/main/src/geopub/database/store.ml
   for an example of the author using their bindings *)

let on_version_change table db =
  ignore @@ Database.create_object_store db table

let open_db table =
  let ovc = on_version_change table in
  Database.open' ~version:db_version ~on_version_change:ovc db_name

let open_os table =
  let tb = Jstr.of_string table in
  let* db = open_db tb in
  let open Transaction in
  let txn = create db ~mode:ReadWrite [tb] in
  let os = object_store txn tb in
  Lwt.return os

let load_table table =
  let* os = open_os table in
  let* xs = ObjectStore.get_all os Jv.null in
  xs
  |> List.map Jv_item.of_obj_jv
  |> List.concat_map Option.to_list
  |> Lwt.return

let save_item table item =
  let* os = open_os table in
  let key = Dohickey.Item.key item |> Jv.of_string in
  let item = Js_common.Jv_item.of_item item in
  let fin = Lwt.return_unit in
  match item with
  | Some item ->
    let* _key = ObjectStore.put os ~key item in
    fin
  | None -> fin

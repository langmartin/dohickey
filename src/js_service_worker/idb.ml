open Brr
open Indexeddb
open Js_common

let db_name = Jstr.of_string "dohickey"

let open_obj_store table =
  let open Lwt.Syntax in
  let open Database in
  let* db = open' ~version:1 db_name in
  let table = Jstr.of_string table in
  let xs = object_store_names db in
  Lwt.return
    begin
      match List.find_opt (Jstr.equal table) xs with
      | Some _x ->
        db
      | None ->
        let _ = create_object_store db table in
        db
    end

let load_table table =
  let open Lwt.Syntax in
  let open Transaction in
  let tb = Jstr.of_string table in
  let* db = open_obj_store table in
  let txn = create db ~mode:ReadOnly [] in
  let os = object_store txn tb in
  ObjectStore.get_all os Jv.null
  |> Lwt.return

let save_item table item =
  let open Lwt.Syntax in
  let open Transaction in
  let tb = Jstr.of_string table in
  let* db = open_obj_store table in
  let txn = create db ~mode:ReadWrite [] in
  let os = object_store txn tb in
  match Js_common.Jv_item.of_item item with
  | Some jv -> ObjectStore.put os jv
  | None -> Lwt.return Jv.null

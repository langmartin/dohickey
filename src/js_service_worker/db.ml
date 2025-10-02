open Indexeddb
open Js_common

let db_name = Jstr.of_string "dohickey"

open Brr

let _open_db table =
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
        Console.debug(["open_db"; "none"; db]);
        let _ = create_object_store db table in
        db
    end

(* See https://github.com/openEngiadina/geopub/blob/main/src/geopub/database/store.ml
   for an example of the author using their bindings *)

let on_version_change table db =
  Database.create_object_store db table

let open_db _table =
  Database.open' ~version:1 db_name

open Lwt.Syntax

let with_open_txn table f =
  let tb = Jstr.of_string table in
  let* db = Database.open' ~version:1 db_name in
  let open Transaction in
  let txn = create db ~mode:ReadWrite [tb] in
  let os = object_store txn tb in
  f os

let load_table table =
  with_open_txn table
    (fun os ->
       let* xs = ObjectStore.get_all os Jv.null in
       xs
       |> List.map Jv_item.of_obj_jv
       |> List.concat_map Option.to_list
       |> Lwt.return)

let save_item table item =
  let open Lwt.Syntax in
  let open Transaction in
  let tb = Jstr.of_string table in
  let* db = open_db tb in

  Console.debug(["store"; db]);

  let txn = create db ~mode:ReadWrite [tb] in

  let os = object_store txn tb in

  Console.debug(["store"; item]);

  match Jv_item.of_item item with
  | Some jv ->
      Console.debug(["store"; jv]);
      ObjectStore.put os jv
  | None -> Lwt.return Jv.null

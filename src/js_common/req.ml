type body =
  | Item of Dohickey.Item.t
  | Dims of int * int
  | Init of string
  | User of string

type t = {
    path: string;
    body: body option
  }

let dims_body jv =
  let xs = Jv.to_list Jv.to_int jv in
  match xs with
  | rows :: cols :: [] -> Some (Dims (rows, cols))
  | _ -> None

let item_body jv =
  match Jv_item.obj_to_item jv with
  | Some item -> Some (Item item)
  | None -> None

let init_body jv =
  match Jv.to_string jv with
  | "" -> None
  | table_id -> Some (Init table_id)

let user_body jv =
  match Jv.to_string jv with
  | "" -> None
  | user -> Some (User user)

let of_dims (rows, cols) =
  { path = "dims"; body = Some (Dims (rows, cols)) }

let of_item item =
  { path = "item"; body = Some (Item item) }

let of_init table_id =
  { path = "init"; body = Some (Init table_id) }

let of_user user =
  { path = "user"; body = Some (User user) }

let of_jv jv =
  let path = Jv.get jv "path" |> Jv.to_string in
  let jv = Jv.get jv "body" in
  match path with
  | "dims" -> {path; body = dims_body jv}
  | "item" -> {path; body = item_body jv}
  | "init" -> {path; body = init_body jv}
  | "user" -> {path; body = user_body jv}
  | _ -> {path; body = None}

let to_jv req =
  let jv_item_body item = Jv_item.of_item item in
  let dims_body row col = Jv.(of_list of_int [row; col]) in
  let init_body table_id = Jv.(of_string table_id) in
  let user_body user = Jv.(of_string user) in

  let jv_req_body req =
    match req.body with
    | Some Item item -> jv_item_body item
    | Some (Dims (row, col)) -> dims_body row col
    | Some Init table_id -> init_body table_id
    | Some User user -> user_body user
    | None -> Jv.null
  in

  Jv.obj [|
    "path", (Jv.of_string req.path);
    "body", (jv_req_body req)
  |]

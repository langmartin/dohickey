type body =
  | Item of Dohickey.Item.t
  | Dims of int * int
  | Title of string
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
  match Jv_item.of_obj_jv jv with
  | Some item -> Some (Item item)
  | None -> None

let title_body jv =
  match Jv.to_string jv with
  | "" -> None
  | title -> Some (Title title)

let user_body jv =
  match Jv.to_string jv with
  | "" -> None
  | user -> Some (User user)

let of_dims (rows, cols) =
  { path = "dims"; body = Some (Dims (rows, cols)) }

let of_item item =
  { path = "item"; body = Some (Item item) }

let of_title title =
  { path = "title"; body = Some (Title title) }

let of_user user =
  { path = "user"; body = Some (User user) }

let of_jv jv =
  let path = Jv.get jv "path" |> Jv.to_string in
  let jv = Jv.get jv "body" in
  match path with
  | "dims" -> {path; body = dims_body jv}
  | "item" -> {path; body = item_body jv}
  | "title" -> {path; body = title_body jv}
  | "user" -> {path; body = user_body jv}
  | _ -> {path; body = None}

let to_jv req =
  let jv_item_body item =
    match Jv_item.of_item item with
    | Some jv -> jv
    | None -> Jv.null
  in
  let dims_body row col = Jv.(of_list of_int [row; col]) in
  let title_body title = Jv.(of_string title) in
  let user_body user = Jv.(of_string user) in

  let jv_req_body req =
    match req.body with
    | Some Item item -> jv_item_body item
    | Some (Dims (row, col)) -> dims_body row col
    | Some Title title -> title_body title
    | Some User user -> user_body user
    | None -> Jv.null
  in

  Jv.obj [|
    "path", (Jv.of_string req.path);
    "body", (jv_req_body req)
  |]

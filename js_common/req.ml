type body =
  | Item of Dohickey.Item.t
  | Dims of int * int

type t = {
    path: string;
    body: body option
  }

let dims_body jv =
  let xs = Jv.get jv "dims" |> Jv.to_list Jv.to_int in
  match xs with
  | rows :: cols :: [] -> Some (Dims (rows, cols))
  | _ -> None

let item_body jv =
  match Jv_item.of_jv jv with
  | Some item -> Some (Item item)
  | None -> None

let of_jv jv =
  let path = Jv.get jv "path" |> Jv.to_string in
  let jv = Jv.get jv "body" in
  match path with
  | "dims" -> {path; body = dims_body jv}
  | "item" -> {path; body = item_body jv}
  | _ -> {path; body = None}

let to_jv req =
  let jv_item_body item =
    match Jv_item.of_item item with
    | Some jv -> jv
    | None -> Jv.null
  in

  let jv_req_body req =
    match req.body with
    | Some Item item -> jv_item_body item
    | Some _ -> Jv.null
    | None -> Jv.null
  in

  Jv.obj [|
    "path", (Jv.of_string req.path);
    "body", (jv_req_body req)
  |]

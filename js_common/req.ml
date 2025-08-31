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

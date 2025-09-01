(*

   The service worker receives incoming messages and turns sends them
   as messages to us, on the client. These receivers draw the updates.

*)

open Brr

let qs querySelector =
  let o = Document.to_jv G.document in
  Jv.call o "querySelectorAll" [| (Jv.of_string querySelector) |]

let len jv =
  Jv.call jv "length" [||] |> Jv.to_int

let rows() = qs "#dohickey tbody tr"
let cols() = qs "#dohickey tbody tr:first-child td"

let make_xs x n =
  let open Seq in
  repeat x
  |> take n
  |> map (fun f -> f())
  |> List.of_seq

let row_el ncols =
  let elf = (fun () -> El.td []) in
  let tds = make_xs elf ncols in
  El.tr tds

let sync_rows n ncols =
  let tb = qs "#dohickey tbody" |> El.of_jv in
  let rows = make_xs (fun () -> row_el ncols) n in
  El.append_children tb rows

let sync_cols n =
  let open List in
  let tdf = (fun () -> El.td []) in
  rows()
  |> Jv.to_list El.of_jv
  |> map (fun tr ->
      make_xs tdf n
      |> El.append_children tr)

(* Received event handlers *)

let dims (row, col) =
  let rn = rows() |> len in
  let cn = cols() |> len in
  sync_rows (row - rn) col;
  sync_cols (col - cn)
  |> ignore

let rec each f xs =
  match xs with
  | [] -> ();
  | x :: xs -> f x; each f xs

let replace_children el kids =
  el
  |> El.children
  |> each El.remove;
  El.append_children el kids

let item_text (body : Dohickey.Item.text_body) =
  let open Dohickey.Item in
  let id = key_text body.row body.col in
  let el = qs id in
  Jv.call el "textContent" [| (Jv.of_string body.text) |]
  |> ignore

let table() =
  match El.find_first_by_selector (Jstr.of_string "#dohickey") with
  | Some el -> el
  | None -> raise (Invalid_argument "document")

let item_call ?(visible=true) (body : Dohickey.Item.call_body) =
  let tb = table() in
  El.fold_find_by_selector
    ~root:tb
    (fun el _ ->
       El.set_class (Jstr.of_string "voting") visible el;
       El.set_at
         (Jstr.of_string "data-call")
         (Some (Jstr.of_string body.id))
         el;)
    (Jstr.of_string ".ballot-box")
    ()

let item (item : Dohickey.Item.t) =
  match item.body with
  | Text it -> item_text it
  | Call it -> item_call it
  | Count it -> item_call ~visible:false it
  | Vote _it -> ()
  | Result _it -> ()

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

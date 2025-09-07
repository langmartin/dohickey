(*

   The service worker receives incoming messages and in turn sends
   them as messages to us, the client. These receivers draw the
   updates.

*)

open Brr

let document_el = G.document |> Document.to_jv |> El.of_jv

let qsa ?(el=document_el) querySelector =
  let o = El.to_jv el in
  [| (Jv.of_string querySelector) |]
  |> Jv.call o "querySelectorAll"
  |> Jv.to_list El.of_jv

let rows() = qsa "#dohickey tbody tr"
let cols() = qsa "#dohickey tbody tr:first-child td"

let repeatedly f n =
  let open Seq in
  repeat f
  |> take n
  |> mapi (fun i f -> f i)
  |> List.of_seq

let vote_btn dir =
  let btn = El.button [El.txt' dir] in
  btn

let set_classes el xs =
  List.fold_left (fun _ (c, yes) ->
      El.set_class (Jstr.v c) yes el)
    ()
    xs

type attr_v = Int of int | Str of string | Drop of bool

let set_attrs el xs =
  let at_v x = match x with
    | Int x -> Some (Jstr.of_int x)
    | Str x -> Some (Jstr.of_string x)
    | Drop false -> None
    | Drop true -> Some (Jstr.v "true")
  in
  List.fold_left (fun _ (c, v) ->
      El.set_at (Jstr.v c) (at_v v) el)
    ()
    xs

(* TODO: on_click event handler that sends a vote *)
let vote_ctx row col =
  let el = El.div ~at:[At.hidden]
      [vote_btn "+"; vote_btn "-"] in
  [("voting", false); ("ballot-box", true);]
  |> set_classes el;

  [("data-row", Int row); ("data-col", Int col)]
  |> set_attrs el;

  el

let add_ev_listener event f el =
  let trg = El.as_target el in
  (* Save this value so we can detatch listeners? *)
  ignore @@ Ev.listen event f trg;
  el

let event_el event =
  event |> Ev.target |> Ev.target_to_jv |> El.of_jv

let rec find_td el =
  match El.parent el with
  | None -> None
  | Some el ->
    let tag = el |> El.tag_name |> Jstr.to_string in
    if tag = "td" then
      Some el
    else
      find_td el

let at_id td =
  match El.at (Jstr.v "id") td with
  | None -> None
  | Some id -> Some (Jstr.to_string id)

let send_text e =
  let el = event_el e in
  let text = el |> El.text_content |> Jstr.to_string in
  (* Like piping each step with |> Option.bind but using fancy syntax *)
  let (>>=) = Option.bind in
  find_td el
  >>= at_id
  >>= Dohickey.Item.parse_pos "text"
  >>= (fun body -> Some (Send.text {body with text=text}))
  |> ignore

let editable txt =
  El.textarea
    [El.txt txt]
  |> add_ev_listener Ev.focusout send_text

let lemme_edit e =
  let el = Ev.target e |> Ev.target_to_jv |> El.of_jv in
  match El.find_first_by_selector ~root:el (Jstr.v ".content") with
  | None -> ()
  | Some ct ->
    let txt = El.text_content ct in
    El.set_children ct
      [editable txt]

(*
   TODO: contenteditable(?) and event handler to send text.
   Also send "typing..."
*)
let dh_td row col =
  let id = Dohickey.Item.key_text row col in
  El.td ~at:[(At.id (Jstr.v id))]
    [vote_ctx row col;
     El.div ~at:[(At.class' (Jstr.v "content"))]
       [El.txt' ""]]
  |> add_ev_listener Ev.click lemme_edit

let sync_cols n =
  let open List in
  rows()
  |> mapi (fun row tr ->
      repeatedly
        (fun col ->
           [dh_td row col]
           |> El.append_children tr)
        n)

let row_el ncols row =
  let elf = dh_td row in
  let tds = repeatedly elf ncols in
  El.tr tds

let sync_rows n ncols =
  match qsa "#dohickey tbody" with
  | [tb] -> repeatedly (row_el ncols) n |> El.append_children tb
  | _ -> ()

let item_text (body : Dohickey.Item.text_body) =
  let open Dohickey.Item in
  let id = key_text body.row body.col in
  let els = ["#"; id; " .content"] |> String.concat "" |> qsa in
  match els with
  | [el] ->
    let el = El.to_jv el in
    ignore @@ Jv.call el "textContent" [| (Jv.of_string body.text) |]
  | _ -> ()

let table() =
  match El.find_first_by_selector (Jstr.of_string "#dohickey") with
  | Some el -> el
  | None -> raise (Invalid_argument "document")

let item_call ?(visible=true) (body : Dohickey.Item.call_body) =
  El.fold_find_by_selector
    ~root:(table())
    (fun el _ ->
       El.set_class (Jstr.of_string "voting") visible el;
       El.set_at
         (Jstr.of_string "data-call")
         (Some (Jstr.of_string body.id))
         el;)
    (Jstr.of_string ".ballot-box")
    ()

(*
   ======================================================================
   Received event handlers
*)

let dims (row, col) =
  let rn = rows() |> List.length in
  let cn = cols() |> List.length in
  sync_rows (row - rn) col;
  ignore @@ sync_cols (col - cn)

let item (item : Dohickey.Item.t) =
  match item.body with
  | Text it -> item_text it
  | Call it -> item_call it
  | Count it -> item_call ~visible:false it
  | Vote _it -> ()
  | Result _it -> ()

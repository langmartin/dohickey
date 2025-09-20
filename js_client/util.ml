open Brr

let document_el = G.document |> Document.to_jv |> El.of_jv

let qsa ?(el=document_el) querySelector =
  let o = El.to_jv el in
  [| (Jv.of_string querySelector) |]
  |> Jv.call o "querySelectorAll"
  |> Jv.to_list El.of_jv

let add_ev_listener event f el =
  let trg = El.as_target el in
  (* Save this value so we can detatch listeners? *)
  ignore @@ Ev.listen event f trg;
  el

let repeatedly f n =
  let open Seq in
  repeat f
  |> take n
  |> mapi (fun i f -> f i)
  |> List.of_seq

let set_classes el xs =
  List.fold_left (fun _ (c, yes) ->
      El.set_class (Jstr.v c) yes el)
    ()
    xs

type attr_v = Int of int | Str of string | Gone

let set_attrs el xs =
  let at_v x = match x with
    | Int x -> Some (Jstr.of_int x)
    | Str x -> Some (Jstr.of_string x)
    | Gone -> None
  in
  List.fold_left (fun _ (c, v) ->
      El.set_at (Jstr.v c) (at_v v) el)
    ()
    xs

let event_el event =
  event |> Ev.target |> Ev.target_to_jv |> El.of_jv

let rec find_parent tag el =
  match El.parent el with
  | None -> None
  | Some el ->
    let tg = el |> El.tag_name |> Jstr.to_string in
    if tg = tag then
      Some el
    else
      find_parent tag el

let at_int prop el =
  match El.at (Jstr.v prop) el with
  | Some s -> int_of_string (Jstr.to_string s)
  | None -> -1

let at_str prop el =
  match El.at (Jstr.v prop) el with
  | Some s -> Jstr.to_string s
  | None -> ""

let (>>=) = Option.bind

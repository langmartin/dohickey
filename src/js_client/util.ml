open Brr

let (>>=) = Option.bind
let (|>>) o f = (Option.map f) o

let document_el = G.document |> Document.to_jv |> El.of_jv

let qsa ?(el=document_el) querySelector =
  let o = El.to_jv el in
  [| Jv.of_string querySelector |]
  |> Jv.call o "querySelectorAll"
  |> Jv.to_list El.of_jv

let qs1 ?(el=document_el) querySelector =
  match qsa ~el:el querySelector with
  | el :: _ -> Some el
  | _ -> None

let qs1_in querySelector el =
  El.find_first_by_selector ~root:el (Jstr.v querySelector)

let add_ev_listener event f el =
  (* Console.debug(["add_ev_listener"; event; el]); *)
  let trg = El.as_target el in
  (* Save this value so we can detatch listeners? *)
  ignore @@ Ev.listen event f trg;
  el

let ev_target_el ev = ev |> Ev.target |> Ev.target_to_jv |> El.of_jv

let ev_submit = Ev.Type.void (Jstr.v "submit")

let el_on_submit f el =
  add_ev_listener ev_submit f el

let on_submit qs f =
  qsa qs |> List.map (el_on_submit f) |> ignore

let el_on_click f el =
  add_ev_listener Ev.click f el

let on_click qs f =
  qsa qs |> List.map (el_on_click f) |> ignore

let on_load thunk =
  (* Wait for page load *)
  ignore @@
  Fut.bind (Ev.next Ev.load (Window.as_target G.window)) @@ fun _ev -> thunk();
  Fut.return()

let repeatedly f n =
  let open Seq in
  repeat f
  |> take n
  |> mapi (fun i f -> f i)
  |> List.of_seq

let each f lst =
  List.fold_left (fun _ x -> f x) () lst

let id' id = At.id (Jstr.v id)
let cls names = At.class' (Jstr.v (String.concat " " names))
let data_row i = At.(int (Jstr.v "data-row") i)
let data_col i = At.(int (Jstr.v "data-col") i)

let set_classes el xs =
  List.fold_left (fun _ (c, yes) ->
      El.set_class (Jstr.v c) yes el)
    ()
    xs

type attr_v = Int of int | Str of string | True | False

let set_attrs el xs =
  let at_v x = match x with
    | Int x -> Some (Jstr.of_int x)
    | Str x -> Some (Jstr.of_string x)
    | True -> Some Jstr.empty
    | False -> None
  in
  List.fold_left (fun _ (c, v) ->
      El.set_at (Jstr.v c) (at_v v) el)
    ()
    xs

let set_at key value el =
  let key = Jstr.of_string key in
  begin
    match value with
      None -> El.set_at key None el
    | Some v -> El.set_at key (Some (Jstr.of_string v)) el
  end;
  el

let event_el event =
  event |> Ev.target |> Ev.target_to_jv |> El.of_jv

let is_tag tags el =
  let tag = el |> El.tag_name |> Jstr.to_string in
  List.mem tag tags

let is_qs qs el = El.find_first_by_selector ~root:el (Jstr.v qs) |> Option.is_some
let is_td el = el |> El.tag_name |> Jstr.to_string |> ( = ) "td"

let rec find_parent is el =
  if is el then
    Some el
  else
    El.parent el >>= find_parent is

let at_int prop el =
  match El.at (Jstr.v prop) el with
  | Some s -> int_of_string (Jstr.to_string s)
  | None -> -1

let at_str prop el =
  match El.at (Jstr.v prop) el with
  | Some s -> Jstr.to_string s
  | None -> ""

let at_bool prop el =
  match El.at (Jstr.v prop) el with
  | Some _ -> true
  | None -> false

let el_prop prop el =
  Jv.get (El.to_jv el) prop

let el_value_content el =
  el_prop "value" el |> Jv.to_string

let el_value_int el =
  (* Jv.of_int is just identity and passed "2" through instead of 2 *)
  el_prop "value" el |> Jv.to_string |> int_of_string

open Brr
open Util
open Dohickey.Util_option

let is_voting() =
  match qs1 "#votey.voting" with None -> false | Some _ -> true

let set_voting on =
  match qs1 "#votey" with None -> () | Some el ->
    El.set_class (Jstr.v "voting") on el

let send_vote row col ev =
  let send_vote row col ev =
    let select = event_el ev in
    let rank = el_value_int select in
    Dohickey.Vote.{row; col; rank} |> Send.vote
  in
  if is_voting() then
    send_vote row col ev

let find_parent_cell_id el =
  let open Util in
  match find_parent is_td el with
    None -> None | Some el ->
    Some (at_str "id" el)

let mark_added el =
  El.set_at (Jstr.v "data-inert") None el

let add_vote_listeners el =
  let id_opt = find_parent_cell_id el >>= Draw_common.parse_id in
  match id_opt with None -> () | Some id ->
    el
    |> add_ev_listener Ev.change (send_vote id.row id.col)
    |> mark_added

(** Add any missing ballot box event handlers *)
let add_all_ballot_boxes() =
  let els = qsa "#dohickey .ballot-box select[data-inert]" in
  List.iter add_vote_listeners els

let set_show_vote is_on =
  qsa "#dohickey td"
  |> List.iter (fun el ->
      El.set_class (Jstr.of_string "voting") is_on el;
      El.set_prop El.Prop.hidden false el)

let rank_cls rank = (Jstr.v ("rank-" ^ (string_of_int rank)))

let clear_all_ranks el =
  [-1; 1; 2; 3; 4; 5]
  |> List.map rank_cls
  |> List.iter (fun c -> El.set_class c false el);
  el

let select_rank rank td =
  (match qs1_in "select" td with None -> () | Some el ->
      El.set_prop El.Prop.value (Jstr.of_int rank) el);
  td

let start() =
  set_voting true;
  add_all_ballot_boxes();
  set_show_vote true

let vote (body:Dohickey.Vote.t) =
  let id = Draw_common.cell_id body.row body.col in
  let qs = id.qs ^ " .vote" in
  match qs1 qs with None -> () | Some el ->
    el
    |> clear_all_ranks
    |> select_rank body.rank
    |> El.set_class (rank_cls body.rank) true

let stop() =
  set_voting false;
  set_show_vote false

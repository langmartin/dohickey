module S = Set.Make(String)

type cursor = {
  mutable user : string;
  mutable users :  S.t;
  mutable pos : int * int;
  mutable edge :  int * int;
  mutable editing : bool;
}

let cursor = {
  users = S.empty;
  user = "";
  pos =  0, 0;
  edge = 1, 1;
  editing = false;
}

let input_buf = ref ""

let set_self user =
  cursor.user <- user

let get_self() =
  cursor.user

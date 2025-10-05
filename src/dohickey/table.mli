module StringMap : Map.S with type key = string
type items = Item.t StringMap.t
type t = { id : string; items : items; }
val empty : t
val make : string -> t
val values : t -> Item.t list
val get_pos : string -> int -> int -> t -> Item.t option
val is_stale : t -> Item.t -> bool
val is_fresh : t -> Item.t -> bool
val replacing : Item.t -> t -> Item.t option
val join : Item.t -> t -> t
val put_list : Item.t list -> t -> t
val dims : t -> int * int
val make_results : string -> string -> string -> t -> Item.t list
val to_list : t -> Item.t list

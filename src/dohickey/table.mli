module StringMap : Map.S with type key = string
type items = Item.t StringMap.t
type t = { id : string; items : items; }
val empty : t
val make : string -> t
val values : t -> Item.t list
val get_pos : string -> int -> int -> t -> Item.t option
val put : Item.t -> t -> t
val puts : Item.t list -> t -> t * Item.t list
val dims : t -> int * int
val make_results : string -> string -> string -> t -> Item.t list
val to_list : t -> Item.t list

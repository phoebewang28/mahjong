type elem
type t

val compare_elem : elem -> elem -> int
val init_hidden_hand : Tile.tile list -> t
val get : t -> int -> elem
val set : t -> int -> Tile.tile -> unit

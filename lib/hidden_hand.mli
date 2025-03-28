type elem
type hidden_hand

val compare_elem : elem -> elem -> int
(** [compare_elem e1 e2] compares the tiles stored at elements [e1] and [e2]. *)

val init_hidden_hand : Tile.tile list -> hidden_hand
(** [init_hidden_hand tiles] initializes the player's hand using a list of tiles
    passed to it. Requires that [tiles] is a list of tiles with length at most
    14. *)

val get : hidden_hand -> int -> elem
(** [get hh idx] returns the tile stored at index [idx]. Raises Failure if there
    is no tile at [idx], or if [idx] is greater than 13 (max hand size is 14).
*)

val get_size : hidden_hand -> int
(** [get_size hh] returns the number of "actual" tiles in the player's hand. *)

val add : hidden_hand -> Tile.tile -> unit
(** [add hh tile] adds Tile [tile] to the player's hand. *)

val remove : hidden_hand -> int -> unit
(** [remove hh idx] removes Tile at index [idx] from the player's hand. *)

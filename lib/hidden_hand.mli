open Tile

type elem = tile

type hidden_hand = {
  mutable hand : elem array;
      (** Array of elements representing the player's hand *)
  mutable size : int;
}

val init_hidden_hand : Tile.tile list -> hidden_hand
(** [init_hidden_hand tiles] initializes the player's hand using a list of tiles
    passed to it. Requires that [tiles] is a list of tiles with length at most
    14. *)

val get : hidden_hand -> int -> tile
(** [get hh idx] returns the tile stored at index [idx]. Raises
    [Invalid_argument] if there is no tile at [idx], or if [idx ] is greater
    than 13 (max hand size is 14). *)

val get_tile_index : hidden_hand -> tile -> int
(** [get_tile hh tile] returns the index of Tile [tile] in the player's hand.
    Raises [Invalid_argument] if [tile] is not in the player's hand. *)

val get_size : hidden_hand -> int
(** [get_size hh] returns the number of "actual" tiles in the player's hand. *)

val add : hidden_hand -> Tile.tile -> unit
(** [add hh tile] adds Tile [tile] to the player's hand. *)

val remove : hidden_hand -> tile -> unit
(** [remove hh tile] removes Tile [tile] from the player's hand. Raises
    [Invalid_argument] if [tile] is not in player's hand. *)

val make_hidden_hand : Tile.tile list -> hidden_hand
(** [make_hidden_hand tiles] creates a hidden hand from a list of tiles. *)

val get_tiles : hidden_hand -> Tile.tile list
(** [get_tiles hh] returns the list of tiles in the player's hand. *)

val hidden_hand_to_string : hidden_hand -> string
(** [hidden_hand_to_string hh] returns a string representation of the player's
    hand. *)

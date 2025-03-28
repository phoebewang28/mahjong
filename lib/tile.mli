type tile
(** Type of mahjong tile *)

type suit
(** Type of suit of mahjong itle *)

type group
(** Group type of mahjong tile *)

val get_num : tile -> int
(** [get_num t] returns number associated with tile [t] *)

val get_tao : tile -> suit
(** [get_tao t] returns tao associated with tile [t] *)

val init_tiles : unit -> tile array
(** [init_tiles] initializes 136 tiles in a given order*)

val deal : player -> tile
(** [deal] deals a tile from the tile array*)

val discarded : tile list ref
(** [discarded] is a list collecting the discarded tiles. It is initialized with
    a random tile for now. [List.hd discarded] is the most recently discarded
    tile.*)

val tiles_arr : tile array
(** [tiles_arr] is an array containing the order of tiles at game play*)

val string_to_tile : string -> tile
(** [string_to_tile str] turns a string [str] into a tile

    TODO: add examples (Phoebe) *)

val make_tile : int -> suit -> tile
(** [make_tile num tao] creates a tile with number [num] and suit [tao]*)

val group_to_string : group -> string
(** [group_to_string] returns string representation of tile's group *)

val make_group : string -> group
(** [make_group str] returns the group with the name corresponding to [str]

    Preconditin: [str] must be (case sensitive) "Shun", "San", "Si", raise o/w
*)

val tile_to_string : tile -> string
(** [tile_to_string t] returns string representation of tile

    Example: ... TODO (Jess))*)

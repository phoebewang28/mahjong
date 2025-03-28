type tile
(** Type of mahjong tile *)

type suit
(** Type of suit of mahjong itle *)

val get_num : tile -> int
(** [get_num t] returns number associated with tile [t] *)

val get_tao : tile -> suit
(** [get_tao t] returns tao associated with tile [t] *)

val discarded : tile list ref
(** [discarded] is a list collecting the discarded tiles. It is initialized with a random tile for now.
[List.hd discarded] is the most recently discarded tile.*)

val tiles_arr : tile array
(** [tiles_arr] is an array containing the order of tiles at game play*)

val string_to_tile : string -> tile
(** [string_to_tile str] turns a string [str] into a tile*)

val make_tile : int -> suit -> tile
(** [make_tile num tao] creates a tile with number [num] and suit [tao]*)

type player
(** Type of player *)
type tile
(** Type of mahjong tile *)

type suit 
(** Type of suit of mahjong itle *)

val get_num : tile -> int
(** [get_num t] returns number associated with tile [t] *)

val get_tao : tile -> suit
(** [get_tao t] returns tao associated with tile [t] *)
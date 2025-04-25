open Tile
open Hidden_hand
open Exposed_hand

type player
(** Type of player *)

val create : string -> int -> player
(** [create index] creates a player with the given [index] and initializes their
    money to 250. *)

val get_name : player -> string
(** [get_name p] returns the name of player [p] *)

val get_index : player -> int
(** [get_index p] returns the index of player [p] in the list of players *)

val get_money : player -> int
(** [get_money p] returns the amount of money of player [p] currently has*)

val get_hidden : player -> hidden_hand
(** [get_hidden p] returns the hidden tiles of player [p] *)

val get_exposed : player -> exposed_hand
(** [get_exposed p] returns the exposed tiles of player [p] *)

val make_player : string -> int -> int -> hidden_hand -> exposed_hand -> player

type player
(** Type of player *)

val players : player list
(** List of all players *)

val create : string -> int -> player
(** [create index] creates a player with the given [index] and initializes their
    money to 250. *)

val get_index : player -> int
(** [get_index p] returns the index of player [p] in the list of players *)

val get_money : player -> int
(** [get_money p] returns the amount of money of player [p] currently has*)

val get_hidden : player -> tile option array
(** [get_hidden p] returns the hidden tiles of player [p] *)

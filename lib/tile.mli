exception InvalidTile of string

type tile
(** Type of mahjong tile *)

(* type da_pai = | Dong | Nan | Xi | Bei | Zhong | Fa | Bai

   type suit = | Tong | Wan | Tiao | Fake | DaPai of da_pai *)

type suit
(** Type of suit of mahjong tile *)

type group
(** Group type of mahjong tile *)

val curr_index : int ref
(** [curr_index] is a reference to the current index of the tile array *)

val shuffle : 'a array -> unit
(**[shuffle] shuffles the tiles, used at initialization*)

val get_num : tile -> int
(** [get_num t] returns number associated with tile [t]
    - Example: if [t] is a tile with number 5, then [get_num t] returns 5
    - Precondition: [t] must be a valid tile. *)

val get_tao : tile -> suit
(** [get_tao t] returns tao associated with tile [t]
    - Example: if [t] is a tile with suit "Tiao", then [get_tao t] returns
      "Tiao"
    - Precondition: [t] must be a valid tile. *)

val init_tiles : unit -> tile array
(** [init_tiles] initializes 136 tiles in a given order*)

val curr_index : int ref
(** [curr_index] is the index marking the next tile that will be put into play*)

val discarded : tile list ref
(** [discarded] is a list collecting the discarded tiles. It is initialized with
    a random tile for now. [List.hd discarded] is the most recently discarded
    tile.
    - Example: if the last discarded tile is a 5 Tiao, then [List.hd discarded]
      returns a tile with number 5 and suit "Tiao".
    - Precondition: [discarded] must be a valid list of tiles. It is used to
      keep track of the discarded tiles during the game. The first tile in the
      list is the most recently discarded tile. *)

val tiles_arr : tile array ref
(** [tiles_arr] is an array containing the order of tiles at game play*)

val string_to_tile : string -> tile
(** [string_to_tile str] turns a string [str] into a tile.
    - Example: if [str] is "5 Tiao", then [string_to_tile str] returns a tile
      with number 5 and suit "Tiao"
    - Example: if [str] is "Bai", then [string_to_tile str] returns a tile with
      suit "Bai" (the number will be set to 0 as default)
    - Precondition: [str] must be a valid string representation of a tile *)

val make_tile : int -> suit -> tile
(** [make_tile num tao] creates a tile with number [num] and suit [tao]
    - Example: if [num] is 5 and [tao] is "Tiao", then [make_tile num tao]
      returns a tile with number 5 and suit "Tiao"
    - Precondition: [num] must be between 1 and 9 (inclusive) for "Tiao",
      "Tong", and "Wan" suits; or must be 0 for "Dong", "Nan", "Xi", "Bei",
      "Bai", "Fa", or "Zhong" those honor tiles(da_pai). If not, raise an
      exception *)

val group_to_string : group -> string
(** [group_to_string] returns string representation of tile's group
    - Example: if the group is "Shun", then [group_to_string] returns "Shun"
    - Precondition: [group] must be a valid group (i.e. "Shun", "San", or "Si")
*)

val make_group : string -> group
(** [make_group str] returns the group with the name corresponding to [str]
    - Example: if [str] is "Shun", then [make_group str] returns the group
      corresponding to "Shun" (consecutive-number set)
    - Example: if [str] is "San", then [make_group str] returns the group
      corresponding to "San" (triplet of identical tiles)
    - Precondition: [str] must be (case sensitive) "Shun", "San", "Si", raise an
      exception if not. *)

val suit_to_string : suit -> string
val string_to_suit : string -> suit

val tile_to_string : tile -> string
(** [tile_to_string t] returns string representation of tile
    - Example: if [t] is a tile with number 5 and suit "Tiao", then
      [tile_to_string t] returns "5 Tiao"
    - Example: if [t] is a tile with suit "Dong"(the number will be set to 0 as
      default), then [tile_to_string t] returns "Dong" *)

val compare_tile : tile -> tile -> int
(** [compare_tile t1 t2] compares two tiles [t1] and [t2]
    - Returns 0 if they are equal, a negative number if [t1] is less than [t2],
      and a positive number if [t1] is greater than [t2]
    - Example: if [t1] is a tile with number 5 and suit "Tiao", and [t2] is a
      tile with number 6 and suit "Tiao", then [compare_tile t1 t2] returns a
      negative number (since 5 < 6)
    - Precondition: both tiles must be valid tiles *)

val fake_tile : tile

type tile
(** Type of mahjong tile *)

type suit
(** Type of suit of mahjong itle *)

type group
(** Group type of mahjong tile *)

val get_num : tile -> int
(** [get_num t] returns number associated with tile [t]
    - Example: if [t] is a tile with number 5, then [get_num t] returns 5 *)

val get_tao : tile -> suit
(** [get_tao t] returns tao associated with tile [t]
    - Example: if [t] is a tile with suit "Tiao", then [get_tao t] returns
      "Tiao" *)

val discarded : tile list ref
(** [discarded] is a list collecting the discarded tiles. It is initialized with
    a random tile for now. [List.hd discarded] is the most recently discarded
    tile.
    - Example: if the last discarded tile is a 5 Tiao, then [List.hd discarded]
      returns a tile with number 5 and suit "Tiao". *)

val tiles_arr : tile array
(** [tiles_arr] is an array containing the order of tiles at game play*)

val string_to_tile : string -> tile
(** [string_to_tile str] turns a string [str] into a tile

    Example [str]: "1 Tong", "Bai" *)

val make_tile : int -> suit -> tile
(** [make_tile num tao] creates a tile with number [num] and suit [tao]*)

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
    - Preconditin: [str] must be (case sensitive) "Shun", "San", "Si", raise o/w
*)

val tile_to_string : tile -> string
(** [tile_to_string t] returns string representation of tile
    - Example: if [t] is a tile with number 5 and suit "Tiao", then
      [tile_to_string t] returns "5 Tiao"
    - Example: if [t] is a tile with suit "Dong"(the number will be set to 0 as
      default), then [tile_to_string t] returns "Dong" *)

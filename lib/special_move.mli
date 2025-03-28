open Tile
open Exposed_hand

val chi : hidden_hand -> exposed_hand -> tile -> exposed_hand
(** Given hidden hand [hid] of player, and most recently discarded tile [tile],
    [chi hid ex tile], if the combination of tiles within [hid] SELECTED by
    player and [tile] is a legal set of 3 consecutive numbers of the same type
    of tile, adds combo to [ex]. Returns updated [ex].

    Side effect: removes legal set from player's hidden hand [hid]*)

val peng : hidden_hand -> exposed_hand -> tile -> exposed_hand
(** Given hidden hand [hid] of player, and most recently discarded tile [tile],
    [peng hid ex tile], if the combination of tiles within [hid] SELECTED by
    player and [tile] is a legal set of 3 IDENTICAL tiles (by structural
    equality), adds combo to [ex]. Returns updated [ex].

    Side effect: removes legal set from player's hidden hand [hid] *)

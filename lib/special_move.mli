val chi : hidden_hand -> exposed_hand -> tile -> bool
(** Given hidden hand [hid] of player, and most recently discarded tile [tile],
    [chi hid ex tile] returns true if the combination of tiles within [hid]
    SELECTED by player and [tile] is a legal set of 3 consecutive numbers of the
    same type of tile.

    Side effect: outputs legal set into player's exposed hand [ex], removes
    legal set from player's hidden hand [hid]*)

val peng : hidden_hand -> exposed_hand -> tile -> bool
(** Given hidden hand [hid] of player, and most recently discarded tile [tile],
    [peng hid ex tile] returns true if the combination of tiles within [hid]
    SELECTED by player and [tile] is a legal set of 3 IDENTICAL tiles (by
    structural equality).

    Side effect: outputs legal set into player's exposed hand [ex], removes
    legal set from player's hidden hand [hid] *)

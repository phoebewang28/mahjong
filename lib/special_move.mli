open Tile
open Exposed_hand

val chi : hidden_hand -> exposed_hand -> exposed_hand
(** [chi hid ex] takes in hidden hand [hid] of player, and exposed hand [ex] of
    player.

    Prompts player to SELECT 2 tiles within [hid]. If selected tiles and most
    recently discarded tile forms a combination that is a valid chi, adds combo
    to [ex]. Returns updated [ex].

    Side effect: removes legal set from player's hidden hand [hid]

    Valid chi: 3 consecutive numbers of the same type of tile. *)

val peng : hidden_hand -> exposed_hand -> exposed_hand
(** [peng hid ex] takes in hidden hand [hid] of player, and exposed hand [ex] of
    player.

    Prompts player to SELECT 2 tiles within [hid]. If selected tiles and most
    recently discarded tile forms a combination that is a valid peng, adds combo
    to [ex]. Returns updated [ex].

    Side effect: removes legal set from player's hidden hand [hid]

    Valid peng: 3 IDENTICAL tiles (by structural equality)
    *)

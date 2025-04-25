open Tile
open Player
open Hidden_hand
open Exposed_hand

(* TODO: update comments!! *)
val draw : player -> tile
(** [draw hid tile] adds the next tile in available tiles to player's hidden
    hand [hid]. *)

val throw : player -> int -> tile
(** Precondition: Assumes [hid] is sorted! (And must resort after drawing!!) For
    GUI usage, after tile in GUI has been clicked. [id] rep. index of tile that
    player desire to throw From player hand, let player choose which tile they
    want to discard *)

val chi_check : hidden_hand -> bool
(** [chi_check hand] returns true if a player's [hand] can chi *)

val peng_check : hidden_hand -> bool
(** [peng_check hand] returns true if a player's [hand] can peng *)

val chi : player -> int -> int -> bool
(** [chi hid ex] takes in hidden hand [hid] of player, and exposed hand [ex] of
    player.

    Prompts player to SELECT 2 tiles within [hid]. If selected tiles and most
    recently discarded tile forms a combination that is a valid chi, adds combo
    to [ex]. Returns updated [ex].

    Side effect: removes legal set from player's hidden hand [hid]

    Valid chi: 3 consecutive numbers of the same type of tile. *)

val peng : player -> int -> int -> bool
(** [peng hid ex] takes in hidden hand [hid] of player, and exposed hand [ex] of
    player.

    Prompts player to SELECT 2 tiles within [hid]. If selected tiles and most
    recently discarded tile forms a combination that is a valid peng, adds combo
    to [ex]. Returns updated [ex].

    Side effect: removes legal set from player's hidden hand [hid]

    Valid peng: 3 IDENTICAL tiles (by structural equality) *)

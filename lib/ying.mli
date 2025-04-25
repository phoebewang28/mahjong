open Player
open Tile
open Hidden_hand
open Exposed_hand

val complete : player -> bool
(** [complete p] checks if player [p] has a complete hand.
    - A complete hand consists of 4 groups (each group can be a Shun or San) and
      1 pair.
    - Returns true if the hand is complete, false otherwise.
    - Precondition: [p] must be a valid player with a hidden hand of at most 14
      tiles. *)

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

val pinghu : player -> bool
(** [pinghu p] checks if player [p] has a Pinghu hand.
    - A Pinghu hand consists of 4 Shun groups and 1 pair.
    - Returns true if the hand is a Pinghu, false otherwise.
    - Precondition: [p] must be a valid player with a hidden hand of at most 14
      tiles. *)

val dasixi : player -> bool
(** [dasixi p] checks if player [p] has a Dasixi hand.
    - A Dasixi hand consists of 4 groups of 4 wind tiles and 1 pair.
    - Returns true if the hand is a Dasixi, false otherwise.
    - Precondition: [p] must be a valid player with a hidden hand of at most 14
      tiles. *)

val dasanyuan : player -> bool
(** [dasanyuan p] checks if player [p] has a Dasanyuan hand.
    - A Dasanyuan hand consists of 3 groups of dragon tiles and 1 pair.
    - Returns true if the hand is a Dasanyuan, false otherwise.
    - Precondition: [p] must be a valid player with a hidden hand of at most 14
      tiles. *)
(* val qingyise : player -> bool *)
open Tile

val draw : hidden_hand -> bool
(** [draw hidden_hand tile] adds the next tile Pop tile from possible tiles and
    add to player hand *)

val throw : hidden_hand -> bool
(* From player hand, let player choose which tile they want to discard *)

val choose_move : player -> bool
(* Asking for user input (e.g. option #1: draw, option #2: chi; option #3: pong)
   Call corresponding functions below *)

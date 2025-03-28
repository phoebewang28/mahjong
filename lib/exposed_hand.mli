open Tile

type exposed_hand
(* Functions to manipulate exposed hands. Each function takes a tile and an
   exposed_hand and returns a new exposed_hand with the specified group
   added. *)

val chi : tile -> exposed_hand -> exposed_hand
(** A chi is a sequence of three tiles in the same suit. The left, middle,
   and right functions specify which tile is being added to the sequence. 
   
   Given tile [t], [chi t] adds consecutive-number set to exposed hand. 
   Example: [t] has number of value 1, numbers of all tiles inset will be = {1,2,3}
   
   Precondition: Only called when legal chi set is created. [t] have must be SMALLEST number. *)

(* Functions to add a peng or ming gang to the exposed_hand. *)
(* A peng is a triplet of the same tile, while a ming gang is a quad. *)

(* Given tile [t], [peng t] adds a triplet of identical tiles to the exposed
   hand. Example: [t] has number of value 1, numbers of all tiles inset will be
   = {1,1,1} *)
val peng : tile -> exposed_hand -> exposed_hand
val ming_gang : tile -> exposed_hand -> exposed_hand

(* Converts an exposed_hand to a string representation for display purposes. *)
val exposed_hand_to_string : exposed_hand -> string

open Tile

type exposed_hand

(* Functions to manipulate exposed hands. Each function takes a tile and an
   exposed_hand and returns a new exposed_hand with the specified group
   added. *)

(** A chi is a sequence of three tiles in the same suit. The left, middle,
   and right functions specify which tile is being added to the sequence. 
   
   Given tile [t], [chi t] adds consecutive-number set to exposed hand. 
   Example: [t] has number of value 1, numbers of all tiles inset will be = {1,2,3}
   
   Precondition: Only called when legal chi set is created. [t] have must be SMALLEST number. *)
val chi : tile -> exposed_hand -> exposed_hand

(* Caedy: Also, for all these fxns it might make more sense to return unit
   instead, cuz the array of the exposed hand will be mutated anyways even if we
   don't return it 
   
   Also, exposed_hand might be an array instead *)

   
(* Functions to add a peng or ming gang to the exposed_hand. *)
(* A peng is a triplet of the same tile, while a ming gang is a quad. *)
val peng : tile -> exposed_hand -> exposed_hand
val ming_gang : tile -> exposed_hand -> exposed_hand

(* Converts an exposed_hand to a string representation for display purposes. *)
val exposed_hand_to_string : exposed_hand -> string

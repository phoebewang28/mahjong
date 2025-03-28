open Tile

type exposed_hand
(* Functions to manipulate exposed hands. Each function takes a tile and an
   exposed_hand and returns a new exposed_hand with the specified group
   added. *)

val chi : tile -> exposed_hand -> exposed_hand
(** Given tile [t], [chi t] adds consecutive-number set to exposed hand. 
   - Example: [t] is 1 Tong, numbers of all tiles inset will be = {1,2,3} while
   the type will be "Tong" and the group will be "Shun".
   - Precondition: Only called when legal chi set is created.*)

val peng : tile -> exposed_hand -> exposed_hand
(** Given tile [t], [peng t] adds a triplet of identical tiles to the exposed
   hand. 
   - Example: [t] is 1 Tong, numbers of all tiles inset will be
   = {1,1,1} while the type will be "Tong" and the group will be "San".
   - Precondition: Only called when legal peng set is created.*)

val ming_gang : tile -> exposed_hand -> exposed_hand
(** Given tile [t], [ming_gang t] adds a quad of identical tiles to the exposed
   hand. 
   - Example: [t] is 1 Tong, numbers of all tiles inset will be
   = {1,1,1,1} while the type will be "Tong" and the group will be "Si".
   - Precondition: Only called when legal peng set is created.*)

val exposed_hand_to_string : exposed_hand -> string
(** [exposed_hand_to_string h] returns a string representation of the exposed
    hand [h].
    - Example: if [h] has a KeZi of 1 Tong and a ShunZi starting with 2 Tiao,
      then the string returned is "1 Tong KeZi, 2 Tiao ShunZi".
    - Precondition: [h] must be a valid exposed_hand. *)

open Tile
open Player
open Hidden_hand
open Exposed_hand

(* TODO: update comments!! *)
val draw : player -> tile
(** [draw hid tile] adds the next tile in available tiles to player's hidden
    hand [hid].

    Precondition: Assumes [hid] is sorted! (And must resort after drawing!!) *)
val throw : player -> elem -> tile
(* From player hand, let player choose which tile they want to discard *)

(* val choose_move : player -> unit *)
(* Asking for user input (e.g. option #1: draw, option #2: chi; option #3: pong)
   Call corresponding functions below *)


val chi_check : hidden_hand -> bool
(** [chi_check hand] returns true if a player's [hand] can chi *)

val peng_check : hidden_hand -> bool
(** [peng_check hand] returns true if a player's [hand] can peng *)

   val chi : player -> elem -> elem -> bool
   (** [chi hid ex] takes in hidden hand [hid] of player, and exposed hand [ex] of
       player.
   
       Prompts player to SELECT 2 tiles within [hid]. If selected tiles and most
       recently discarded tile forms a combination that is a valid chi, adds combo
       to [ex]. Returns updated [ex].
   
       Side effect: removes legal set from player's hidden hand [hid]
   
       Valid chi: 3 consecutive numbers of the same type of tile. *)
   
   val peng : player -> elem -> elem -> bool
   (** [peng hid ex] takes in hidden hand [hid] of player, and exposed hand [ex] of
       player.
   
       Prompts player to SELECT 2 tiles within [hid]. If selected tiles and most
       recently discarded tile forms a combination that is a valid peng, adds combo
       to [ex]. Returns updated [ex].
   
       Side effect: removes legal set from player's hidden hand [hid]
   
       Valid peng: 3 IDENTICAL tiles (by structural equality) *)

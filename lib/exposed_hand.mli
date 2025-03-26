type group =
  | Shun
  | San
  | Si

type da_pai =
  | Dong
  | Nan
  | Xi
  | Bei
  | Zhong
  | Fa
  | Bai

type suit =
  | Tong
  | Wan
  | Tiao
  | DaPai of da_pai

type tile = {
  num : int;
  tao : suit;
}

(* A.F.: list of p groups [(g1, n1);...; (gp, np)] represents legal sets in
   player’s exposed hand R.I.: p (size of list) ≤ 4 *)
type exposed_hand = (group * tile) list

(* Functions to manipulate exposed hands. Each function takes a tile and an
   exposed_hand and returns a new exposed_hand with the specified group
   added. *)

(* Caedy: My implementation of chi will just return the lowest number in the
   set, cuz u can infer from the lowest number, what the rest of the tiles are
   (precondition: already removed from hidden hand -> so only need to output to
   exposed)*)

(* Chi functions for left, middle, and right tiles. *)
(* A chi is a sequence of three tiles in the same suit. The left, middle,
   and right functions specify which tile is being added to the sequence. *)
val chi_left : tile -> exposed_hand -> exposed_hand
val chi_middle : tile -> exposed_hand -> exposed_hand
val chi_right : tile -> exposed_hand -> exposed_hand

(* Functions to add a peng or ming gang to the exposed_hand. *)
(* A peng is a triplet of the same tile, while a ming gang is a quad. *)
val peng : tile -> exposed_hand -> exposed_hand
val ming_gang : tile -> exposed_hand -> exposed_hand

(* Converts an exposed_hand to a string representation for display purposes. *)
val exposed_hand_to_string : exposed_hand -> string

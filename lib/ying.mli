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

val lvyise : player -> bool
(** [lvyise p] checks if player [p] has a Lvyise hand.
    - A Lvyise hand is a complete hand consisting of 2,3,4,6,8 tiao and Fa.
    - Returns true if the hand is a Lvyise, false otherwise.
    - Precondition: [p] must be a valid player with a hidden hand of at most 14
      tiles. *)

val qidui : player -> bool
(** [qidui p] checks if player [p] has a Qidui hand.
    - A Qidui hand consists of 7 pairs of tiles.
    - Returns true if the hand is a Qidui, false otherwise.
    - Precondition: [p] must be a valid player with a hidden hand of at most 14
      tiles. *)

val jiulianbaodeng : player -> bool
(** [jiulianbaodeng p] checks if player [p] has a Jiulianbaodeng hand.
    - A Jiulianbaodeng hand consists of a consecutive sequece of 7 pairs of a single suite.
    - Returns true if the hand is a Jiulianbaodeng, false otherwise.
    - Precondition: [p] must be a valid player with a hidden hand of at most 14
      tiles. *)

val duiduihu : player -> bool
(** [duiduihu p] checks if player [p] has a Duiduihu hand.
    - A Duiduihu hand consists of 4 Kezi groups and 1 pair.
    - Returns true if the hand is a Duiduihu, false otherwise.
    - Precondition: [p] must be a valid player with a hidden hand of at most 14
      tiles. *)
val kankanhu : player -> bool
(** [kankanhu p] checks if player [p] has a Kankanhu hand.
    - A Kankanhu hand consists of 4 Kezi groups and 1 pair, and all Kezi is in hidden hand.
    - Returns true if the hand is a Kankanhu, false otherwise.
    - Precondition: [p] must be a valid player with a hidden hand of at most 14
      tiles. *)

val qingyise : player -> bool
(** [qingyise p] checks if player [p] has a Qingyise hand.
    - A Qingyise hand consists of 4 groups of a single suite and 1 pair.
    - Returns true if the hand is a Qingyise, false otherwise.
    - Precondition: [p] must be a valid player with a hidden hand of at most 14
      tiles. *)

val ziyise : player -> bool
(** [ziyise p] checks if player [p] has a Ziyise hand.
    - A Ziyise hand consists of all zi(Dapai).
    - Returns true if the hand is a Ziyise, false otherwise.
    - Precondition: [p] must be a valid player with a hidden hand of at most 14
      tiles. *)

val hunyise : player -> bool
(** [hunyiise p] checks if player [p] has a Hunyiise hand.
    - A Hunyiise hand consists a single suite of Wan, Tong, Tiao and Dapai.
    - Returns true if the hand is a Hunyiise, false otherwise.
    - Precondition: [p] must be a valid player with a hidden hand of at most 14
      tiles. *)

val sananke : player -> bool
(** [sananke p] checks if player [p] has a Sananke hand.
    - A Sananke hand is a complete hand consists of 3 groups of 3 identical tiles in hidden hand.
    - Returns true if the hand is a Sananke, false otherwise.
    - Precondition: [p] must be a valid player with a hidden hand of at most 14
      tiles.*)

val xiaosixi : player -> bool
(** [xiaosixi p] checks if player [p] has a Xiaosixi hand.
    - A Xiaosixi hand is a complete hand consists of 3 groups of the 4 wind tiles and 1 pair of wind tiles.
    - Returns true if the hand is a Xiaosixi, false otherwise.
    - Precondition: [p] must be a valid player with a hidden hand of at most 14
      tiles. *)

val duanyaojiu : player -> bool
(** [duanyaojiu p] checks if player [p] has a Duanyaojiu hand.
    - A Duanyaojiu hand is a complete hand without tiles of num 1 and 9.
    - Returns true if the hand is a Duanyaojiu, false otherwise.
    - Precondition: [p] must be a valid player with a hidden hand of at most 14
      tiles. *)

val qingyaojiu : player -> bool
(** [qingyaojiu p] checks if player [p] has a Qingyaojiu hand.
    - A Qingyaojiu hand is a complete hand consists of only num 1 and 9.
    - Returns true if the hand is a Qingyaojiu, false otherwise.
    - Precondition: [p] must be a valid player with a hidden hand of at most 14
      tiles.*)

val hunyaojiu : player -> bool
(** [hunyaojiu p] checks if player [p] has a Hunyaojiu hand.
    - A Hunyaojiu hand is a complete hand consists of only num 1 and 9 and zi(Dapai).
    - Returns true if the hand is a Hunyaojiu, false otherwise.
    - Precondition: [p] must be a valid player with a hidden hand of at most 14
      tiles. *)
open Mahjong.Exposed_hand
open Mahjong

let () =
  let tiles = Tile.tiles_arr in
  let player1 = 
    {
      index : int;
      mutable money : int;
      mutable hidden : tile option array;
      mutable exposed : (group * int) list;
    }



  let hand = chi_left { num = 1; tao = Tong } [] in
  let hand = chi_middle { num = 2; tao = Tong } hand in
  let hand = chi_right { num = 3; tao = Tong } hand in
  let hand = peng { num = 4; tao = Wan } hand in
  let hand = peng { num = 0; tao = DaPai Zhong } hand in
  let hand = ming_gang { num = 5; tao = Tiao } hand in
  print_endline (exposed_hand_to_string hand)

(* This code is a simple test for the exposed_hand module. It creates a hand
   with various combinations of tiles and prints the resulting hand as a string.
   The exposed_hand_to_string function is assumed to be defined in the
   Exposed_hand module, which formats the hand for display. *)
  let s = read_line () in  
  print_endline "Player 1: "^s ;;

open Mahjong

(**[make_player] initializes a player with a certain index*)
let make_player id =
  print_string ("Player " ^ string_of_int id ^ ": ");
  let p = read_line () in
  let player = Player.create p id in
  player

(** [print_hand] nicely prints out game information: tiles in [player]'s hand,

    exposed and hidden*)
let print_hand player =
  print_endline (String.make 100 '-');
  print_endline
    ("Player "
    ^ string_of_int (Player.get_index player)
    ^ ": " ^ Player.get_name player);
  print_endline "Hidden tiles:";
  print_endline (Hidden_hand.hidden_hand_to_string (Player.get_hidden player));
  print_endline "";
  print_endline "Exposed tiles:";
  print_endline
    (Exposed_hand.exposed_hand_to_string (Player.get_exposed player))

(* note: does not ever change player rn lol ... *)
let rec move p1 p2 p3 p4 : unit =
  print_hand p1;
  Player_choice.choose_move p1;
  print_hand p2;
  Player_choice.choose_move p2;
  print_hand p3;
  Player_choice.choose_move p3;
  print_hand p4;
  Player_choice.choose_move p4;
  move p1 p2 p3 p4

let () =
  (* Initialize the players' hands with tiles from the shuffled deck *)
  Random.self_init ();
  print_endline
    "Welcome to OCaMahJong made by love with Lao Gan [O Ca] Ma Jiang!";
  print_endline "Please enter your 4 player names: ";

  Tile.init_tiles ();
  let player1 = make_player 1 in
  let player2 = make_player 2 in
  let player3 = make_player 3 in
  let player4 = make_player 4 in
  print_endline "Get ready to play Mahjong!";
  (*Prints out player hands for debugging sake*)
  print_hand player1;
  print_hand player2;
  print_hand player3;
  print_hand player4;
  (*Recursively calls moves for players in order*)
  move player1 player2 player3 player4

(* Expose some tiles for player 1 as an example *)

(* This code is a simple test for the exposed_hand module. It creates a hand
   with various combinations of tiles and prints the resulting hand as a string.
   The exposed_hand_to_string function is assumed to be defined in the
   Exposed_hand module, which formats the hand for display. *)

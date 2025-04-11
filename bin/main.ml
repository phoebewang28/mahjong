open Mahjong
open ANSITerminal

(** [make_player] initializes a player with a certain index *)
let make_player id =
  ANSITerminal.printf [ yellow ] "Player %d: " id;
  let p = read_line () in
  let player = Player.create p id in
  player

(** [print_hand] nicely prints out game information: tiles in [player]'s hand,
    exposed and hidden *)
let print_hand player =
  ANSITerminal.printf [ cyan ] "%s\n" (String.make 100 '-');
  ANSITerminal.printf [ yellow ] "Player %d: %s\n" (Player.get_index player)
    (Player.get_name player);
  ANSITerminal.printf [ magenta ] "Hidden tiles:\n";
  ANSITerminal.printf [ green ] "%s\n"
    (Hidden_hand.hidden_hand_to_string (Player.get_hidden player));
  print_endline "";
  ANSITerminal.printf [ magenta ] "Exposed tiles:\n";
  ANSITerminal.printf [ green ] "%s\n"
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
  ANSITerminal.printf [ cyan ]
    "Welcome to OCaMahJong made by love with Lao Gan [O Ca] Ma Jiang!\n";
  ANSITerminal.printf [ yellow ] "Please enter your 4 player names:\n";

  let _ = Tile.init_tiles () in
  (* Tile.shuffle !Tile.tiles_arr; *)
  let player1 = make_player 1 in
  let player2 = make_player 2 in
  let player3 = make_player 3 in
  let player4 = make_player 4 in

  ANSITerminal.printf [ red ] "Get ready to play Mahjong!\n";

  (* Prints out player hands for debugging sake *)
  print_hand player1;
  print_hand player2;
  print_hand player3;
  print_hand player4;

  (* Recursively calls moves for players in order *)
  move player1 player2 player3 player4

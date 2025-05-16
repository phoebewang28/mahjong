open Mahjong
open ANSITerminal

(** prints separators between players *)
let print_spacing player =
  ANSITerminal.printf [ cyan ] "%s\n" (String.make 100 '-');
  ANSITerminal.printf [ yellow ] "Player %d: %s\n" (Player.get_index player)
    (Player.get_name player)

(** [print_hand] nicely prints out game information: tiles in [player]'s hand,
    exposed and hidden *)
let print_hand player =
  ANSITerminal.printf [ magenta ] "Hidden tiles:\n";
  ANSITerminal.printf [ green ] "%s\n"
    (Hidden_hand.hidden_hand_to_string (Player.get_hidden player));
  print_endline "";
  ANSITerminal.printf [ magenta ] "Exposed tiles:\n";
  ANSITerminal.printf [ green ] "%s\n"
    (Exposed_hand.exposed_hand_to_string (Player.get_exposed player))

(** prompts user for input, num decides if they choose 1 or 2 numbers. returns
    tuple of the numbers selected. if only 1 needs to be selected, the other
    value is -1*)
let rec prompt_ind hid num : int * int =
  let len = Hidden_hand.get_size hid in
  if num = 1 then (
    ANSITerminal.printf [ yellow ]
      "Please discard 1 child: Enter number from 0 - %d: " (len - 1);
    try
      let ind = int_of_string (String.trim (Stdlib.read_line ())) in
      if ind >= 0 && ind <= len - 1 then (ind, -1)
      else (
        ANSITerminal.printf [ blue ]
          "At least one tile selected does not fall within valid bounds\n\
           Please reselect: ";
        prompt_ind hid 1)
    with
    | Failure e when e = "int_of_string" ->
        ANSITerminal.printf [ yellow ]
          "Tiles selected are invalid.\nPlease reselect: ";
        prompt_ind hid 1
    | Failure e ->
        ANSITerminal.printf [ yellow ] "%s" e;
        prompt_ind hid 1)
  else if num = 2 then (
    ANSITerminal.printf [ blue ]
      "Please choose 2 tiles from your hand: Enter number from 0 - %d: "
      (len - 1);
    try
      let ind1 = Stdlib.read_int () in
      let ind2 = Stdlib.read_int () in
      if (ind1 >= 0 && ind1 <= len - 1) && ind2 >= 0 && ind2 <= len - 1 then
        (ind1, ind2)
      else (
        ANSITerminal.printf [ red ]
          "At least one tile selected does not fall within valid bounds\n\
           Please reselect: ";
        prompt_ind hid 2)
    with
    | Failure e when e = "int_of_string" ->
        ANSITerminal.printf [ red ]
          "Tiles selected are invalid.\nPlease reselect: ";
        prompt_ind hid 2
    | Failure e ->
        ANSITerminal.printf [ red ] "%s" e;
        prompt_ind hid 2)
  else invalid_arg "num must be 1 or 2"

(** lets player choose their move *)
let rec choose_move player =
  let invalid_choice move msg =
    begin
      ANSITerminal.printf [ red ] "Cannot %s, %s\n" move msg;
      choose_move player
    end
  in
  ANSITerminal.printf [ blue ] "Last Discard: %s\n\n"
    (Tile.tile_to_string (List.hd !Tile.discarded));
  ANSITerminal.printf [ yellow ]
    "choose a move: option #1: draw, option #2: chi; option #3: peng\n";
  let choice = read_line () in
  let choice_tile = Str.split (Str.regexp " ") choice in
  (match List.hd choice_tile with
  | "draw" ->
      let drawn = Player_choice.draw player in
      ANSITerminal.printf [ green ] "You drew %s\n" (Tile.tile_to_string drawn);
      print_hand player;
      let t, _ = prompt_ind (Player.get_hidden player) 1 in
      let thrown = Player_choice.throw player t in
      ANSITerminal.printf [ green ] "You threw %s\n"
        (Tile.tile_to_string thrown);
      ()
  | "chi" ->
      if Player_choice.chi_check (Player.get_hidden player) then (
        print_hand player;
        let t1, t2 = prompt_ind (Player.get_hidden player) 2 in
        if not (Player_choice.chi player t1 t2) then
          invalid_choice "chi" "please choose valid tiles"
        else (
          print_hand player;
          let t, _ = prompt_ind (Player.get_hidden player) 1 in
          let thrown = Player_choice.throw player t in
          ANSITerminal.printf [ green ] "You threw %s\n"
            (Tile.tile_to_string thrown);
          ()))
      else invalid_choice "chi" "please choose again"
  | "peng" ->
      if Player_choice.peng_check (Player.get_hidden player) then (
        print_hand player;
        let t1, t2 = prompt_ind (Player.get_hidden player) 2 in
        if not (Player_choice.peng player t1 t2) then
          invalid_choice "peng" "please choose valid tiles"
        else (
          print_hand player;
          let t, _ = prompt_ind (Player.get_hidden player) 1 in
          let thrown = Player_choice.throw player t in
          ANSITerminal.printf [ green ] "You threw %s\n"
            (Tile.tile_to_string thrown);
          ()))
      else invalid_choice "peng" "please choose again"
  | _ -> choose_move player);
  print_hand player

(** [make_player] initializes a player with a certain index *)
let make_player id =
  ANSITerminal.printf [ yellow ] "Player %d: " id;
  let p = read_line () in
  let player = Player.create p id in
  player

(* note: does not ever change player rn lol ... *)
let rec move p1 p2 p3 p4 : unit =
  print_spacing p1;
  print_hand p1;
  choose_move p1;
  if Ying.pinghu p1 then print_endline "Player 1 won!";

  print_spacing p2;
  print_hand p2;
  choose_move p2;
  if Ying.pinghu p1 then print_endline "Player 2 won!";

  print_spacing p3;
  print_hand p3;
  choose_move p3;
  if Ying.pinghu p1 then print_endline "Player 3 won!";

  print_spacing p4;
  print_hand p4;
  choose_move p4;
  if Ying.pinghu p1 then print_endline "Player 4 won!";

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
  print_spacing player1;
  print_hand player1;

  print_spacing player2;
  print_hand player2;

  print_spacing player3;
  print_hand player3;

  print_spacing player4;
  print_hand player4;

  (* Recursively calls moves for players in order *)
  try move player1 player2 player3 player4 with
  | Tile.NoTileLeft -> print_endline "Game ended without a winner..."
  | Ying.PlayerWin player -> print_endline (Player.get_name player ^ " won!")

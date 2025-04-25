open Mahjong
open ANSITerminal

(** [print_hand] nicely prints out game information: tiles in [player]'s hand,
    exposed and hidden *)

let print_spacing player =
  ANSITerminal.printf [ cyan ] "%s\n" (String.make 100 '-');
  ANSITerminal.printf [ yellow ] "Player %d: %s\n" (Player.get_index player)
    (Player.get_name player)

let print_hand player =
  ANSITerminal.printf [ magenta ] "Hidden tiles:\n";
  ANSITerminal.printf [ green ] "%s\n"
    (Hidden_hand.hidden_hand_to_string (Player.get_hidden player));
  print_endline "";
  ANSITerminal.printf [ magenta ] "Exposed tiles:\n";
  ANSITerminal.printf [ green ] "%s\n"
    (Exposed_hand.exposed_hand_to_string (Player.get_exposed player))

let rec select_tiles (lb, ub) hidden_hand =
  try
    let t1 = int_of_string (String.trim (Stdlib.read_line ())) in
    if t1 >= lb && t1 <= ub then Hidden_hand.get hidden_hand t1
    else (
      ANSITerminal.printf [ blue ]
        "At least one tile selected does not fall within valid bounds\n\
         Please reselect: ";
      select_tiles (lb, ub) hidden_hand)
  with
  | Failure e when e = "int_of_string" ->
      ANSITerminal.printf [ yellow ]
        "Tiles selected are invalid.\nPlease reselect: ";
      select_tiles (lb, ub) hidden_hand
  | Failure e ->
      ANSITerminal.printf [ yellow ] "%s" e;
      select_tiles (lb, ub) hidden_hand

let prompt_selection hid =
  let len = Hidden_hand.get_size hid in
  ANSITerminal.printf [ yellow ]
    "Please discard 1 child: Enter number from 0 - %d: " (len - 1);
  select_tiles (0, len - 1) hid

let rec select_tiles_2 (lb, ub) hid =
  try
    let t1 = Stdlib.read_int () in
    let t2 = Stdlib.read_int () in
    if (t1 >= lb && t1 <= ub) && t2 >= lb && t2 <= ub then
      (Hidden_hand.get hid t1, Hidden_hand.get hid t2)
    else (
      ANSITerminal.printf [ red ]
        "At least one tile selected does not fall within valid bounds\n\
         Please reselect: ";
      select_tiles_2 (lb, ub) hid)
  with
  | Failure e when e = "int_of_string" ->
      ANSITerminal.printf [ red ]
        "Tiles selected are invalid.\nPlease reselect: ";
      select_tiles_2 (lb, ub) hid
  | Failure e ->
      ANSITerminal.printf [ red ] "%s" e;
      select_tiles_2 (lb, ub) hid

let prompt_selection_2 hid =
  let len = Hidden_hand.get_size hid in
  ANSITerminal.printf [ blue ]
    "Please choose 2 tiles from your hand: Enter number from 0 - %d: " (len - 1);
  select_tiles_2 (0, len - 1) hid

let rec choose_move player =
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
      let t = prompt_selection (Player.get_hidden player) in
      let thrown = Player_choice.throw player t in
      ANSITerminal.printf [ green ] "You threw %s\n"
        (Tile.tile_to_string thrown);
      ()
  | "chi" ->
      if Player_choice.chi_check (Player.get_hidden player) then (
        print_hand player;
        let t1, t2 = prompt_selection_2 (Player.get_hidden player) in
        let _ = Player_choice.chi player t1 t2 in
        print_hand player;
        let t = prompt_selection (Player.get_hidden player) in
        let thrown = Player_choice.throw player t in
        ANSITerminal.printf [ green ] "You threw %s\n"
          (Tile.tile_to_string thrown);
        ())
      else begin
        ANSITerminal.printf [ red ] "Cannot chi, please choose again\n";
        choose_move player
        (* if legal, allows throw, if not re-prompt player to choose move *)
      end
  | "peng" ->
      if Player_choice.peng_check (Player.get_hidden player) then (
        print_hand player;
        let t1, t2 = prompt_selection_2 (Player.get_hidden player) in
        let _ = Player_choice.peng player t1 t2 in
        print_hand player;
        let t = prompt_selection (Player.get_hidden player) in
        let thrown = Player_choice.throw player t in
        ANSITerminal.printf [ green ] "You threw %s\n"
          (Tile.tile_to_string thrown);
        ())
      else begin
        ANSITerminal.printf [ red ] "Cannot peng, please choose again\n";
        choose_move player
      end
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

  print_spacing p2;
  print_hand p2;
  choose_move p2;

  print_spacing p3;
  print_hand p3;
  choose_move p3;

  print_spacing p4;
  print_hand p4;
  choose_move p4;

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
  move player1 player2 player3 player4

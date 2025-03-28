open Mahjong

let () =
  print_endline "Welcome to Mahjong!";
  print_endline "Please enter your name: ";
  let p1 = read_line () in  
  let player1 = Player.create p1 1 in
  print_endline ("Player 1: " ^ Player.get_name player1);
  let p2 = read_line () in
  let player2 = Player.create p2 2 in
  print_endline ("Player 2: " ^ player2.name);
  let p3 = read_line () in
  let player3 = Player.create p3 3 in
  print_endline ("Player 3: " ^ player3.name);
  let p4 = read_line () in
  let player4 = Player.create p4 4 in
  print_endline ("Player 4: " ^ player4.name);
  print_endline "Get ready to play Mahjong!";
  print_endline "Dealing tiles...";

  let tiles_arr = Tile.init_tiles () in

  


  let hand = chi_left { num = 1; tao = Tong } [] in
  let hand = chi_middle { num = 2; tao = Tong } hand in
  let hand = chi_right { num = 3; tao = Tong } hand in
  let hand = peng { num = 4; tao = Wan } hand in
  let hand = peng { num = 0; tao = DaPai Zhong } hand in
  let hand = ming_gang { num = 5; tao = Tiao } hand in
  print_endline (exposed_hand_to_string hand)

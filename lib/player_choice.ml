open Tile
open Str
open Player
open Hidden_hand
open ANSITerminal

let rec select_tiles (lb, ub) hidden_hand : tile =
  try
    let t1 = int_of_string (String.trim (Stdlib.read_line ())) in
    if t1 >= lb && t1 <= ub then get hidden_hand t1
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
  let len = get_size hid in
  ANSITerminal.printf [ yellow ]
    "Please discard 1 child: Enter number from 0 - %d: " (len - 1);
  select_tiles (0, len - 1) hid

let throw (player : player) : unit =
  let hid = get_hidden player in
  let tile = prompt_selection hid in
  ANSITerminal.printf [ yellow ] "\nPlayer %d: *%s* is discarding tile [%s]!\n"
    (Player.get_index player) (Player.get_name player)
    (Tile.tile_to_string tile);
  discarded := tile :: !discarded;
  Hidden_hand.remove hid tile

let throw_with_index (player : player) id : unit =
  let hid = get_hidden player in
  let tile = Hidden_hand.get hid id in
  ANSITerminal.printf [ yellow ] "\nPlayer %d: *%s* is discarding tile [%s]!\n"
    (Player.get_index player) (Player.get_name player)
    (Tile.tile_to_string tile);
  discarded := tile :: !discarded;
  Hidden_hand.remove hid tile

let draw (player : player) =
  let hidden_hand = get_hidden player in
  let tile = Array.get !tiles_arr !curr_index in
  ANSITerminal.printf [ blue ] "\nPlayer %d: *%s* is drawing tile [%s]!\n"
    (Player.get_index player) (Player.get_name player)
    (Tile.tile_to_string tile);
  add hidden_hand tile;
  curr_index := !curr_index + 1;
  ANSITerminal.printf [ blue ] "Tile added at index: %d\n"
    (get_tile_index hidden_hand tile);
  tile

let comp_tiles t1 t2 = get_num t1 - get_num t2

let rec is_consec lst =
  match lst with
  | [] -> true
  | [ a; b ] -> if get_num a + 1 <> get_num b then false else true
  | x :: xs :: t ->
      if get_num x + 1 <> get_num xs then false else is_consec (xs :: t)
  | _ -> failwith "Cannot check consecutivity for list of length < 2!"

let print_player_hid_exp player =
  ANSITerminal.printf [ magenta ] "New Hidden Tiles:\n";
  ANSITerminal.printf [ green ] "%s\n"
    (Hidden_hand.hidden_hand_to_string (Player.get_hidden player));
  ANSITerminal.printf [ magenta ] "New Exposed Tiles:\n";
  ANSITerminal.printf [ green ] "%s\n"
    (Exposed_hand.exposed_hand_to_string (Player.get_exposed player))

let chi_update t1 t2 dis ex hid : bool =
  if not (get_tao t1 = get_tao t2 || get_tao t2 = get_tao dis) then false
  else
    let sorted = List.sort comp_tiles [ t1; t2; dis ] in
    if not (is_consec sorted) then false
    else (
      Hidden_hand.remove hid t1;
      Hidden_hand.remove hid t2;
      Exposed_hand.chi (List.hd sorted) ex;
      true)

let peng_update t1 t2 dis ex hid : bool =
  if not (get_tao t1 = get_tao t2 || get_tao t2 = get_tao dis) then false
  else if not (get_num t1 = get_num t2 || get_num t2 = get_num dis) then false
  else (
    Hidden_hand.remove hid t1;
    Hidden_hand.remove hid t2;
    Exposed_hand.peng t1 ex;
    true)

let rec select_tiles_2 (lb, ub) hid : tile * tile =
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

let chi_check (hand : hidden_hand) : bool =
  let dis = List.hd !discarded in
  let pair_check t1 t2 =
    try
      let _ = get_tile_index hand t1 in
      let _ = get_tile_index hand t2 in
      true
    with Invalid_argument _ -> false
  in
  let dis_num = get_num dis in
  if dis_num < 1 || dis_num > 9 then false
  else
    (* array: [| l2 (dis_num -2), l1 (dis_num - 1), u2 (dis_num + 2), u1
       (dis_num + 1)||] 
       
       Necessary tests: l2 & l1, l1 & u1 (aka. discard in the middle), u1 & u2 *)
    let combo_arr = [| None; None; None; None |] in
    if dis_num - 2 >= 1 then
      combo_arr.(0) <- Some (make_tile (dis_num - 2) (get_tao dis));
    if dis_num - 1 >= 1 then
      combo_arr.(1) <- Some (make_tile (dis_num - 1) (get_tao dis));
    if dis_num + 2 <= 9 then
      combo_arr.(2) <- Some (make_tile (dis_num + 2) (get_tao dis));
    if dis_num + 1 <= 9 then
      combo_arr.(3) <- Some (make_tile (dis_num + 1) (get_tao dis));  
    (* Array.iter (fun x -> match x with 
    | None -> print_endline "None; "
    | Some t -> print_endline (Tile.tile_to_string t)) combo_arr;
    print_newline (); *)
    match combo_arr with
    | [| Some l2; Some l1; Some u2; Some u1|] -> pair_check l2 l1 || pair_check l1 u1 || pair_check u2 u1
    | [| None; Some l1; Some u2; Some u1|] -> pair_check l1 u1 || pair_check u2 u1
    | [| None; None; Some u2; Some u1|] -> pair_check u2 u1
    | [| Some l2; Some l1; None; Some u1|] -> pair_check l2 l1 || pair_check l1 u1
    | [| Some l2; Some l1; None; None|] -> pair_check l2 l1 
    | _ -> false 

let peng_check (hand : hidden_hand) : bool =
  let dis = List.hd !discarded in
  let rec count_tiles h acc =
    match h with
    | [] -> acc
    | h :: t ->
        if h = dis then count_tiles t (acc + 1) else count_tiles t acc
  in
  if count_tiles (get_tiles hand) 0 >= 2 then true else false

let chi (player : player) : bool =
  let hid = get_hidden player in
  let ex = get_exposed player in
  let sel = prompt_selection_2 hid in
  let dis = List.hd !discarded in
  chi_update (fst sel) (snd sel) dis ex hid

let chi_with_index (player : player) id1 id2 : bool =
  let hid = get_hidden player in
  let ex = get_exposed player in
  let dis = List.hd !discarded in
  chi_update (Hidden_hand.get hid id1) (Hidden_hand.get hid id2) dis ex hid

let peng_with_index (player : player) id1 id2 : bool =
  let hid = get_hidden player in
  let ex = get_exposed player in
  let dis = List.hd !discarded in
  peng_update (Hidden_hand.get hid id1) (Hidden_hand.get hid id2) dis ex hid

let peng (player : player) : bool =
  let hid = get_hidden player in
  let ex = get_exposed player in
  let sel = prompt_selection_2 hid in
  let dis = List.hd !discarded in
  peng_update (fst sel) (snd sel) dis ex hid

let rec choose_move player =
  ANSITerminal.printf [ blue ] "Last Discard: %s\n\n"
    (Tile.tile_to_string (List.hd !discarded));
  ANSITerminal.printf [ yellow ]
    "choose a move: option #1: draw, option #2: chi; option #3: peng\n";
  let choice = read_line () in
  let choice_tile = Str.split (Str.regexp " ") choice in
  (match List.hd choice_tile with
  | "draw" ->
      let _ = draw player in
      throw player
  | "chi" ->
      if chi player then (
        print_player_hid_exp player;
        throw player)
      else begin
        ANSITerminal.printf [ red ] "Cannot chi, please choose again\n";
        choose_move player
        (* if legal, allows throw, if not re-prompt player to choose move *)
      end
  | "peng" ->
      if peng player then (
        print_player_hid_exp player;
        throw player)
      else begin
        print_endline "Cannot peng, please choose again\n";
        choose_move player
      end
  | _ -> choose_move player);
  print_player_hid_exp player

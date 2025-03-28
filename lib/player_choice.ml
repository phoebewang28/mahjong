open Tile
open Str
open Player
open Hidden_hand

(** Prompts user for input selection of 2 tiles (user selectes INDEX of tile).
    Returns tuple of tiles corresponding to selected indexes if valid, o/w
    re-prompt user.

    Valid input must be: An integer within [(lb, ub)] where [lb] rep. lower
    bound (inclusive) and [ub] rep. upper bound (inclusive).

    Postcondition: returns valid tile *)
let rec select_tiles (lb, ub) hidden_hand : tile =
  try
    let t1 = int_of_string (String.trim (Stdlib.read_line ())) in
    if t1 >= lb && t1 <= ub then get hidden_hand t1
    else (
      print_string
        "At least one tile selected does not fall within valid bounds\n\
         Please reselect: ";
      select_tiles (lb, ub) hidden_hand)
  with
  | Failure e when e = "int_of_string" ->
      print_string "Tiles selected are invalid.\nPlease reselect: ";
      select_tiles (lb, ub) hidden_hand
  | Failure e ->
      print_string e;
      select_tiles (lb, ub) hidden_hand
(* aka. reruns when invalid input given *)

(** Effect: Displays prompt in terminal to initiate user selection of tiles,
    returns selected 2 tiles *)
let prompt_selection hid =
  (* prompts player for selection of tiles from [hid] *)
  let len = get_size hid in
  print_string
    ("Please discard 1 child: Enter number from 0 - "
    ^ string_of_int (len - 1)
    ^ ": ");
  (* for now, prompt appears in terminal, will update when GUI implemented *)
  select_tiles (0, len - 1) hid

let throw (player : player) : unit =
  let hid = get_hidden player in
  let tile = prompt_selection hid in
  print_endline
    ("\nPlayer "
    ^ string_of_int (Player.get_index player)
    ^ ": *" ^ Player.get_name player ^ "* is discarding tile ["
    ^ Tile.tile_to_string tile ^ "]!\n");
  discarded := make_tile (get_num tile) (get_tao tile) :: !discarded;
  Hidden_hand.remove hid tile

let draw (player : player) : unit =
  let hidden_hand = get_hidden player in
  let tile = Array.get !tiles_arr !curr_index in
  print_endline
    ("\nPlayer "
    ^ string_of_int (Player.get_index player)
    ^ ": *" ^ Player.get_name player ^ "* is drawing tile ["
    ^ Tile.tile_to_string tile ^ "]!\n");
  add hidden_hand tile;
  (* update current index *)
  curr_index := !curr_index + 1;
  print_endline
    ("Tile added at index: " ^ string_of_int (get_tile hidden_hand tile));
  throw player

(** Effect: [comp_tiles t1 t2] returns

    - 0 if [t1] is equal to [t2]
    - negative if [t1] is less than [t2]
    - positive if [t1] is greater than [t2] *)
let comp_tiles t1 t2 = get_num t1 - get_num t2

(** Effect: [is_consec lst] returns true if for list [lst] [t1; t;, ...; tn],
    all elements have an integer value ONE greater than the previous

    Precondition: [lst] must be ≥ 2, raises o/w *)
let rec is_consec lst =
  match lst with
  | [] -> true
  | [ a; b ] -> if get_num a + 1 <> get_num b then false else true
  | x :: xs :: t ->
      if get_num x + 1 <> get_num xs then false else is_consec (xs :: t)
  | _ -> failwith "Cannot check consecutivity for list of length < 2!"

let print_player_hid_exp player =
  print_endline "New Hidden Tiles:";
  print_endline
    (Hidden_hand.hidden_hand_to_string (Player.get_hidden player) ^ "\n");
  print_endline "New Exposed Tiles:";
  print_endline
    (Exposed_hand.exposed_hand_to_string (Player.get_exposed player) ^ "\n")

(** Effect: Given two tiles from player's hidden hand [t1] & [t2], and discarded
    tile [dis], checks if combination is a legal "chi".

    Side effects: If combo is legal, removes from hidden hand.

    Returns: updated exposed hand [ex] after adding legal set to it*)
let chi_update t1 t2 dis ex hid : bool =
  if not (get_tao t1 = get_tao t2 || get_tao t2 = get_tao dis) then false
    (* returns exposed_hand unchanged *)
  else
    let sorted = List.sort comp_tiles [ t1; t2; dis ] in
    if not (is_consec sorted) then false
    else (
      (* must move the index!! *)
      Hidden_hand.remove hid t1;
      Hidden_hand.remove hid t2;
      (* returns exposed_hand, after adding set to it *)
      Exposed_hand.chi (List.hd sorted) ex;
      true)

(** Effect: Given two tiles from player's hidden hand [t1] & [t2] and discarded
    tile [dis], checl if combination is a legal "peng" (aka. all tiles have same
    suit and same number).

    Side effects: If combo is legal, removes from hidden hand.

    Returns: updated exposed hand [ex] after adding legal set to it Edge case:
    da_pai *)
let peng_update t1 t2 dis ex hid : bool =
  if not (get_tao t1 = get_tao t2 || get_tao t2 = get_tao dis) then false
  else if not (get_num t1 = get_num t2 || get_num t2 = get_num dis) then false
  else (
    Hidden_hand.remove hid t1;
    Hidden_hand.remove hid t2;
    Exposed_hand.peng t1 ex;
    true)

(** Prompts user for input selection of 2 tiles (user selectes INDEX of tile).
    Returns tuple of tiles corresponding to selected indexes if valid, o/w
    re-prompt user.

    Valid input must be: An integer within [(lb, ub)] where [lb] rep. lower
    bound (inclusive) and [ub] rep. upper bound (inclusive).

    Postcondition: returns valid tile *)
let rec select_tiles_2 (lb, ub) hid : tile * tile =
  try
    let t1 = Stdlib.read_int () in
    let t2 = Stdlib.read_int () in
    if (t1 >= lb && t1 <= ub) && t2 >= lb && t2 <= ub then
      (Hidden_hand.get hid t1, Hidden_hand.get hid t2)
    else (
      print_string
        "At least one tile selected does not fall within valid bounds\n\
         Please reselect: ";
      select_tiles_2 (lb, ub) hid)
  with
  | Failure e when e = "int_of_string" ->
      print_string "Tiles selected are invalid.\nPlease reselect: ";
      select_tiles_2 (lb, ub) hid
  | Failure e ->
      print_string e;
      select_tiles_2 (lb, ub) hid
(* aka. reruns when invalid input given *)

(** Effect: Displays prompt in terminal to initiate user selection of tiles,
    returns selected 2 tiles *)
let prompt_selection_2 hid =
  (* prompts player for selection of tiles from [hid] *)
  let len = Hidden_hand.get_size hid in
  print_string
    ("Please choose 2 tiles from your hand: Enter number from 0 - "
    ^ string_of_int (len - 1)
    ^ ": ");
  (* for now, prompt appears in terminal, will update when GUI implemented *)
  select_tiles_2 (0, len - 1) hid

let chi (player : player) : bool =
  let hid = get_hidden player in
  let ex = get_exposed player in
  let sel = prompt_selection_2 hid in
  let dis = List.hd !discarded in
  (* most recently discarded tile *)
  chi_update (fst sel) (snd sel) dis ex hid

let peng (player : player) : bool =
  let hid = get_hidden player in
  let ex = get_exposed player in
  let sel = prompt_selection_2 hid in
  let dis = List.hd !discarded in
  peng_update (fst sel) (snd sel) dis ex hid

let rec choose_move player =
  print_string "Last Discard: ";
  print_endline (Tile.tile_to_string (List.hd !discarded) ^ "\n");
  print_endline
    "choose a move: option #1: draw, option #2: chi; option #3: peng";
  let choice = read_line () in
  (* type out the move the player wants to do*)
  let choice_tile = Str.split (Str.regexp " ") choice in
  (match List.hd choice_tile with
  | "draw" -> draw player
  | "chi" ->
      if chi player then (
        print_player_hid_exp player;
        throw player)
      else choose_move player
        (* if legal, allows throw, if not re-prompt player to choose move *)
  | "peng" ->
      if peng player then (
        print_player_hid_exp player;
        throw player)
      else choose_move player
  | _ -> choose_move player);
  print_player_hid_exp player
(* player input wrong, repeatedly asks player until right input*)

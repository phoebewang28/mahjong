open Tile
open Str
open Player
open Hidden_hand

let throw (player : player) thrown =
  let hid = get_hidden player in
  discarded := thrown :: !discarded;
  Hidden_hand.remove hid thrown;
  thrown

let draw (player : player) =
  let hidden_hand = get_hidden player in
  let tile = Array.get !tiles_arr !curr_index in
  add hidden_hand tile;
  curr_index := !curr_index + 1;
  tile

(* ?? i thought i saw another compare function in another file*)
let comp_tiles t1 t2 = get_num t1 - get_num t2

let rec is_consec lst =
  match lst with
  | [] -> true
  | [ a; b ] -> if get_num a + 1 <> get_num b then false else true
  | x :: xs :: t ->
      if get_num x + 1 <> get_num xs then false else is_consec (xs :: t)
  | _ -> failwith "Cannot check consecutivity for list of length < 2!"

(* should these be in player ml?*)
let print_player_hid player =
  Hidden_hand.hidden_hand_to_string (Player.get_hidden player)

let print_player_exp player =
  Exposed_hand.exposed_hand_to_string (Player.get_exposed player)

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

       Necessary tests: l2 & l1, l1 & u1 (aka. discard in the middle), u1 &
       u2 *)
    let combo_arr = [| None; None; None; None |] in
    if dis_num - 2 >= 1 then
      combo_arr.(0) <- Some (make_tile (dis_num - 2) (get_tao dis));
    if dis_num - 1 >= 1 then
      combo_arr.(1) <- Some (make_tile (dis_num - 1) (get_tao dis));
    if dis_num + 2 <= 9 then
      combo_arr.(2) <- Some (make_tile (dis_num + 2) (get_tao dis));
    if dis_num + 1 <= 9 then
      combo_arr.(3) <- Some (make_tile (dis_num + 1) (get_tao dis));
    (* Array.iter (fun x -> match x with | None -> print_endline "None; " | Some
       t -> print_endline (Tile.tile_to_string t)) combo_arr; print_newline
       (); *)
    match combo_arr with
    | [| Some l2; Some l1; Some u2; Some u1 |] ->
        pair_check l2 l1 || pair_check l1 u1 || pair_check u2 u1
    | [| None; Some l1; Some u2; Some u1 |] ->
        pair_check l1 u1 || pair_check u2 u1
    | [| None; None; Some u2; Some u1 |] -> pair_check u2 u1
    | [| Some l2; Some l1; None; Some u1 |] ->
        pair_check l2 l1 || pair_check l1 u1
    | [| Some l2; Some l1; None; None |] -> pair_check l2 l1
    | _ -> false

let peng_check (hand : hidden_hand) : bool =
  let dis = List.hd !discarded in
  let rec count_tiles h acc =
  let rec count_tiles h acc =
    match h with
    | [] -> acc
    | h :: t -> if h = dis then count_tiles t (acc + 1) else count_tiles t acc
  in
  if count_tiles (get_tiles hand) 0 >= 2 then true else false
  if count_tiles (get_tiles hand) 0 >= 2 then true else false

let chi (player : player) t1 t2 : bool =
  let hid = get_hidden player in
  let ex = get_exposed player in
  let dis = List.hd !discarded in
  chi_update t1 t2 dis ex hid

let peng (player : player) t1 t2 : bool =
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
  let dis = List.hd !discarded in
  peng_update t1 t2 dis ex hid

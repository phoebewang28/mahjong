open Tile
open Exposed_hand

(** Effect: [comp_tiles t1 t2] returns

    - 0 if [t1] is equal to [t2]
    - negative if [t1] is less than [t2]
    - positive if [t1] is greater than [t2] *)
let comp_tiles t1 t2 = get_num t1 - get_num t2

(** Effect: [is_consec lst] returns true if for list [lst] [t1; t;, ...; tn],
    all elements have an integer value ONE greater than the previous

    Precondition: [lst] must be ≥ 2, raises o/w *)
let rec is_consec = function
  | [] -> true
  | x :: xs :: t ->
      if get_num x + 1 <> get_num xs then false else is_consec (xs :: t)
  | _ -> failwith "Cannot check consecutivity for list of length < 2!"

(** Effect: Given two tiles from player's hidden hand [t1] & [t2], and discarded
    tile [dis], checks if combination is a legal "chi".

    Side effects: If combo is legal, removes from hidden hand.

    Returns: updated exposed hand [ex] after adding legal set to it*)
let chi_update t1 t2 dis ex : exposed_hand =
  if not (get_tao t1 = get_tao t2 && get_tao t2 = get_tao dis) then ex
    (* returns exposed_hand unchanged *)
  else
    let sorted = List.sort comp_tiles [ t1; t2; dis ] in
    if not (is_consec sorted) then ex
    else (
      Hidden_hand.remove_chi (List.hd sorted);
      (* returns exposed_hand, after adding set to it *)
      Exposed_hand.chi (List.hd sorted) ex)

(** Effect: Given two tiles from player's hidden hand [t1] & [t2] and discarded
    tile [dis], checl if combination is a legal "peng" (aka. all tiles have same
    suit and same number).

    Side effects: If combo is legal, removes from hidden hand.

    Returns: updated exposed hand [ex] after adding legal set to it Edge case:
    da_pai *)
let peng_update t1 t2 dis ex : exposed_hand =
  if not (get_tao t1 = get_tao t2 && get_tao t2 = get_tao dis) then ex
  else if not (get_num t1 = get_num t2 && get_num t2 = get_num dis) then ex
  else (
    Hidden_hand.remove_peng t1;
    Exposed_hand.peng t1 ex)

(** Prompts user for input selection of 2 tiles (user selectes INDEX of tile).
    Returns tuple of tiles corresponding to selected indexes if valid, o/w
    re-prompt user.

    Valid input must be: An integer within [(lb, ub)] where [lb] rep. lower
    bound (inclusive) and [ub] rep. upper bound (inclusive).

    Note: Assuming hidden hand has fxn to get tile based on indexes *)
let rec select_tiles (lb, ub) : tile * tile =
  try
    let t1 = Stdlib.read_int () in
    let t2 = Stdlib.read_int () in
    if (t1 >= lb && t1 <= ub) && t2 >= lb && t2 <= ub then
      (Hidden_hand.get_tile t1, Hidden_hand.get_tile t2)
    else
      failwith
        "At least one tile selected does not fall within valid bounds\n\
         Please reselect: "
  with
  | Failure e when e = "int_of_string" ->
      print_string "Tiles selected are invalid.\nPlease reselect: ";
      select_tiles (lb, ub)
  | Failure e ->
      print_string e;
      select_tiles (lb, ub)
(* aka. reruns when invalid input given *)

(** Effect: Displays prompt in terminal to initiate user selection of tiles,
    returns selected 2 tiles *)
let prompt_selection hid =
  (* prompts player for selection of tiles from [hid] *)
  let len = Array.length hid in
  print_string
    ("Please choose 2 tiles from your hand: Enter number from 0 - "
    ^ string_of_int (len - 1));
  (* for now, prompt appears in terminal, will update when GUI implemented *)
  select_tiles (0, len - 1)

let chi hid ex dis : exposed_hand =
  let sel = prompt_selection hid in
  chi_update (fst sel) (snd sel) dis ex

let peng hid ex dis : exposed_hand =
  let sel = prompt_selection hid in
  peng_update (fst sel) (snd sel) dis ex

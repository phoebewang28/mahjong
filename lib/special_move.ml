(** Effect: [comp_tiles t1 t2] returns

    - 0 if [t1] is equal to [t2]
    - negative if [t1] is less than [t2]
    - positive if [t1] is greater than [t2] *)
let comp_tiles t1 t2 = Basic_types.get_num t1 - Basic_types.get_num t2

(** Effect: [is_consec lst] returns true if for list [lst] [t1; t;, ...; tn],
    all elements have an integer value ONE greater than the previous

    Precondition: [lst] must be ≥ 2, raises o/w *)
let rec is_consec = function
  | [] -> true
  | x :: xs :: t ->
      if Basic_types.get_num x + 1 <> Basic_types.get_num xs then false
      else is_consec (xs :: t)
  | _ -> failwith "Cannot check consecutivity for list of length < 2!"

(** Effect: Given two tiles from player's hidden hand [t1] & [t2], and discarded
    tile [dis], return true if combination is a legal "chi".

    Side effects: If combo is legal, removes from hidden hand and adds to
    exposed hand. *)
let chi_update t1 t2 dis : bool =
  if
    not
      (Basic_types.get_tao t1 = Basic_types.get_tao t2
      && Basic_types.get_tao t2 = Basic_types.get_tao dis)
  then false
  else
    let sorted = List.sort comp_tiles [ t1; t2; dis ] in
    if not (is_consec sorted) then false
    else (
      Exposed_hand.chi_left (List.hd sorted);
      (* works only if Exposed_hand.peng updates a mutable array instead of
         returning an immutable list *)
      Hidden_hand.remove_chi (List.hd sorted);
      true)

(** Effect: Given two tiles from player's hidden hand [t1] & [t2], and discarded
    tile [dis], return true if combination is a legal "peng" (aka. all tiles
    have same suit and same number).

    Side effects: If combo is legal, removes from hidden hand and adds to
    exposed hand.

    Edge case: da_pai *)
let peng_update t1 t2 dis : bool =
  if
    not
      (Basic_types.get_tao t1 = Basic_types.get_tao t2
      && Basic_types.get_tao t2 = Basic_types.get_tao dis)
  then false
  else if
    not
      (Basic_types.get_num t1 = Basic_types.get_num t2
      && Basic_types.get_num t2 = Basic_types.get_num dis)
  then false
  else (
    Exposed_hand.peng (List.hd sorted);
    Hidden_hand.remove_peng (List.hd sorted);
    true)

(** Prompts user for input selection of 2 tiles (user selectes INDEX of tile).
    Returns tuple of tiles corresponding to selected indexes if valid, o/w
    re-prompt user.

    Valid input must be: An integer within [(lb, ub)] where [lb] rep. lower
    bound (inclusive) and [ub] rep. upper bound (inclusive).

    Note: Assuming hidden hand has fxn to get tile based on indexes *)
let rec select_tiles (lb, ub) : Basic_types.tile * Basic_types.tile =
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

  
let chi hid ex dis : bool =
  let sel = prompt_selection hid in
  chi_update (fst sel) (snd sel) dis

let peng hid ex dis : bool =
  let sel = prompt_selection hid in
  peng_update (fst sel) (snd sel) dis

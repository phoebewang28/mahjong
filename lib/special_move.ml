open Mahjong

(** Assuming that we have a most_recently_discarded fxn to get most recently discarded tile (look @ doc: within Tiles module)*)

(** Effect: [comp_tiles t1 t2] returns

    - 0 if [t1] is equal to [t2]
    - negative if [t1] is less than [t2]
    - positive if [t1] is greater than [t2] *)
let comp_tiles (t1 : tile) (t2 : tile) =
  Basic_types.get_num t1 - Basic_types.get_num t2

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
let chi_update (t1 : tile) (t2 : tile) (dis : tile) : bool =
  if
    not
      (Basic_types.get_tao t1 = Basic_types.get_tao t2
     = Basic_types.get_tao disc)
  then false
  else
    let sorted = List.sort comp_tiles (t1 :: t2 :: dis) in
    if not (is_consec sorted) then false
    else (
      Exposed_hand.chi_left (List.hd sorted);
      Hidden_hand.remove_chi (List.hd sorted) true)

(** Prompts user for input selection of 2 tiles (user selectes INDEX of tile).
    Returns if selection of indexes if valid, o/w re-prompt user. *)
let rec select_tiles () : tile * tile =
  try
    let t1 = Stdlib.read_int () in
    let t2 = Stdlib.read_int () in
    (t1, t2)
  with Failure e when e = "int_of_string" ->
    print_string "Tile selected is invalid.\n Please reselect: ";
    select_tiles ()
(* aka. reruns when invalid input given *)


(* Note: based on doc, hidden_hand is of type [int option array]

   to output, call Jess' fxns, give number of smallest tile *)
let chi (hid : hidden_hand) (ex : exposed_hand) (dis : tile) : bool =
  (* prompts player for selection of tiles from [hid] *)
  let len = Array.length hid in
  print_string
    ("Please choose 2 tiles from your hand: Enter number from 1 - "
   ^ string_of_int len);
  (* for now, prompt appears in terminal, will update when GUI implemented *)
  let (sel : tile * tile) = select_tiles () in
  chi_update (fst sel) (snd sel) dis

  
let peng (hid : hidden_hand) (ex : exposed_hand) (dis : tile) : bool = true

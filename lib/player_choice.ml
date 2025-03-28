
open Tile
open Str

let draw (hidden_hand:tile option array) = 
  let tile = Array.get tiles_arr !curr_index in
  hidden_hand.(13) <- Some (tile);
  true

  (** Prompts user for input selection of 2 tiles (user selectes INDEX of tile).
    Returns tuple of tiles corresponding to selected indexes if valid, o/w
    re-prompt user.

    Valid input must be: An integer within [(lb, ub)] where [lb] rep. lower
    bound (inclusive) and [ub] rep. upper bound (inclusive).

    Note: Assuming hidden hand has fxn to get tile based on indexes *)
    let rec select_tiles (lb, ub) : tile =
      try
        let t1 = Stdlib.read_int () in
        if (t1 >= lb && t1 <= ub) then
          Hidden_hand.get_tile t1
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

let throw (hand:tile option array) = 
  let tile = prompt_selection hand in
  let ind = Array.find_index (fun x -> x = Some(tile)) hand in 
  match ind with 
    | None -> false (** if the tile they don't want to remove isn't in their hand*)
    | Some i -> match hand.(i) with 
      | None -> false
      | Some t -> discarded := [(make_tile (get_num t) (get_tao t))];   
    hand.(i) <- None; 
    true

    (* filler functions, delete later *)
let chi player  = true
let pong player  = true


let choose_move player = 
  print_endline ("choose a move: option #1: draw, option #2: chi; option #3: pong");
  let choice = read_line() in  (* type out the move the player wants to do*)
  let choice_tile = Str.split (Str.regexp " ") choice in
    if List.hd choice_tile = "draw" then 
      draw player (* Queue.pop available_tiles*)
    else if List.hd choice_tile = "chi" then 
      chi player 
    else if List.hd choice_tile = "pong" then 
      pong player 
    else 
      false
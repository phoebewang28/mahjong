
open Basic_types
open Str

let draw (hidden_hand:tile option array) (tile:tile) = 
  hidden_hand.(13) <- Some (tile);
  true

let throw (hand:tile option array) (tile:tile) = 
  let ind = Array.find_index (fun x -> x = Some(tile)) hand in 
  match ind with 
    | None -> false (** if the tile they don't want to remove isn't in their hand*)
    | Some i -> hand.(i) <- None; true

let chi player tile = true
let pong player tile = true


let choose_move player = 
  print_endline ("choose a move: option #1: draw, option #2: chi; option #3: pong");
  let choice = read_line() in  (* type out the move the player wants to do*)
  let choice_tile = Str.split (Str.regexp " ") choice in
    if List.hd choice_tile = "draw" then 
      let tile = List.hd (List.tl choice_tile) in
      draw player (Basic_types.string_to_tile tile) (* Queue.pop available_tiles*)
    else if List.hd choice_tile = "chi" then 
      let tile = List.hd (List.tl choice_tile) in
      chi player (Basic_types.string_to_tile tile) (* Queue.peek discarded_tiles*)
    else if List.hd choice_tile = "pong" then 
      let tile = List.hd (List.tl choice_tile) in
      pong player (Basic_types.string_to_tile tile) (* Queue.peek discarded_tiles*)
    else 
      false
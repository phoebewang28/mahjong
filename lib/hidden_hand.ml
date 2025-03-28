open Tile

type elem = tile option
(** The type of each element in the hidden hand. None corresponds to the absence
    of a tile. *)

type hidden_hand = {
  hand : elem array;
  mutable size : int;
}
(** The type of the player's hidden hand includes an array of tiles and a size
    corresponding to the number of "actual" tiles in hand. *)

let compare_elem e1 e2 =
  match (e1, e2) with
  | None, None -> 0
  | None, _ -> 1
  | _, None -> -1
  | Some t1, Some t2 -> compare_tile t1 t2

let force_RI hand = Array.sort compare_elem hand

let init_hidden_hand tiles =
  let hand = Array.make 14 None in
  let _ =
    List.fold_left
      (fun acc x ->
        hand.(acc) <- Some x;
        acc + 1)
      0 tiles
  in
  force_RI hand;
  { hand; size = List.length tiles }

let get hh idx =
  match hh.hand.(idx) with
  | None -> raise (Invalid_argument "Index out of bounds")
  | Some x -> x

let get_size hh = hh.size

let add hh t =
  hh.hand.(hh.size) <- Some t;
  force_RI hh.hand;
  hh.size <- hh.size + 1

let remove hh idx =
  hh.hand.(idx) <- None;
  force_RI hh.hand;
  hh.size <- hh.size - 1

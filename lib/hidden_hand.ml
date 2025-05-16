open Tile
open Printf

type elem = tile
(** The type of each element in the hidden hand. None corresponds to the absence
    of a tile. *)

type hidden_hand = {
  mutable hand : elem array;
  mutable size : int;
}
(** The type of the player's hidden hand includes an array of tiles and a size
    corresponding to the number of "actual" tiles in hand. *)

(* let compare_elem e1 e2 = match (e1, e2) with | None, None -> 0 | None, _ -> 1
   | _, None -> -1 | Some t1, Some t2 -> compare_tile t1 t2 *)

let sort hand = Array.sort compare_tile hand

let init_hidden_hand tiles =
  let size = List.length tiles in
  let hand = Array.make size fake_tile in
  let _ =
    List.fold_left
      (fun acc x ->
        hand.(acc) <- x;
        acc + 1)
      0 tiles
  in
  sort hand;
  { hand; size }

let get hh idx = hh.hand.(idx)

let get_tile_index hh tile =
  match Array.find_index (fun x -> x = tile) hh.hand with
  | None -> raise (Invalid_argument "Index out of bounds")
  | Some x -> x

let get_size hh = hh.size

let add hh t =
  hh.hand <- Array.concat [ hh.hand; Array.make 1 t ];
  sort hh.hand;
  hh.size <- hh.size + 1

let remove hh tile =
  match Array.find_index (fun x -> x = tile) hh.hand with
  | None -> raise (Invalid_argument "Tile does not exist")
  | Some idx ->
      hh.hand.(idx) <- fake_tile;
      sort hh.hand;
      hh.size <- hh.size - 1

let get_hand hh = hh.hand

let make_hidden_hand hand =
  { hand = Array.of_list hand; size = List.length hand }

let get_tiles hh = Array.to_list hh.hand

let hidden_hand_to_string hh =
  let s =
    List.fold_left
      (fun acc x -> acc ^ ", " ^ Tile.tile_to_string x)
      "" (get_tiles hh)
  in
  String.sub s 1 (String.length s - 1)
[@@coverage off]

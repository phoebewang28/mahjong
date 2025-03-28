open Tile

(* Basic Type implementation with comparison function (to be merged) *)
type da_pai =
  | Dong
  | Nan
  | Xi
  | Bei
  | Zhong
  | Fa
  | Bai

type suit =
  | Tong
  | Wan
  | Tiao
  | DaPai of da_pai

(* R.I: for tiles of da_pai, num is always set to 0 *)
type tile = {
  num : int;
  tao : suit;
}

let prio_by_suit t =
  match t.tao with
  | Tong -> 1
  | Wan -> 2
  | Tiao -> 3
  | _ -> 4

(** [compare_tile t1 t2] compares tiles [t1] and [t2] by suit and number. Suit
    priority is determined by [prio_by_suit], and same-suit tiles are then
    sorted by number in increasing numerical order. *)
let compare_tile t1 t2 =
  if prio_by_suit t1 != prio_by_suit t2 then prio_by_suit t1 - prio_by_suit t2
  else t1.num - t2.num

type elem = tile option

type t = {
  hand : elem array;
  size : int;
}

let compare_elem e1 e2 =
  match (e1, e2) with
  | None, None -> 0
  | None, _ -> -1
  | _, None -> 1
  | Some t1, Some t2 -> compare_tile t1 t2

let init_hidden_hand tiles =
  { hand = Array.of_list (List.map (fun x -> Some x) tiles); size = 14 }

let force_RI hand = Array.sort compare_tile hand
let get hh idx = hh.hand.(idx)
let set hh idx t = hh.hand.(idx) <- Some t

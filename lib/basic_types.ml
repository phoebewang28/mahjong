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

let get_num (t : tile) = t.num
let get_tao (t : tile) = t.tao

type group =
  | Shun
  | San
  | Si

type player = {
  index : int;
  mutable money : int;
  mutable hidden : tile option array;
  mutable exposed : (group * int) list;
}
(** RI: hidden is size 14, exposed is size 4, index is from 0-3*)

let string_to_tile str =
  let t = Str.split (Str.regexp " ") str in
  match List.hd t with
  | "Tong" -> { num = int_of_string (List.hd (List.tl t)); tao = Tong }
  | "Wan" -> { num = int_of_string (List.hd (List.tl t)); tao = Wan }
  | "Tiao" -> { num = int_of_string (List.hd (List.tl t)); tao = Tiao }
  | "Dong" -> { num = int_of_string (List.hd (List.tl t)); tao = DaPai Dong }
  | "Nan" -> { num = int_of_string (List.hd (List.tl t)); tao = DaPai Nan }
  | "Xi" -> { num = int_of_string (List.hd (List.tl t)); tao = DaPai Xi }
  | "Bei" -> { num = int_of_string (List.hd (List.tl t)); tao = DaPai Bei }
  | "Zhong" -> { num = int_of_string (List.hd (List.tl t)); tao = DaPai Zhong }
  | "Fa" -> { num = int_of_string (List.hd (List.tl t)); tao = DaPai Fa }
  | "Bai" -> { num = int_of_string (List.hd (List.tl t)); tao = DaPai Bai }
  | _ -> { num = 0; tao = Tong }

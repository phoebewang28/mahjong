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

type group =
  | Shun
  | San
  | Si

(* R.I: for tiles of da_pai, num is always set to 0 *)
type tile = {
  num : int;
  tao : suit;
}

let get_num (t : tile) = t.num
let get_tao (t : tile) = t.tao
let curr_index = ref 0

(**[init_tiles] initializes 136 tiles in a given order*)
let init_tiles () =
  let tiles = Array.make 136 { num = 1; tao = Tong } in
  let index = ref 0 in

  (*adds 4 copies of any tile*)
  let add_tiles number suit =
    for _ = 1 to 4 do
      tiles.(!index) <- { num = number; tao = suit };
      incr index
    done
  in
  (*adds the 3 suited tiles with associated numbers*)
  let add_by_suit suit =
    for i = 1 to 9 do
      add_tiles i suit
    done
  in
  add_by_suit Tong;
  add_by_suit Wan;
  add_by_suit Tiao;

  (*adds the four winds*)
  add_tiles 0 (DaPai Dong);
  add_tiles 0 (DaPai Nan);
  add_tiles 0 (DaPai Xi);
  add_tiles 0 (DaPai Bei);

  (*adds the three dragons*)
  add_tiles 0 (DaPai Zhong);
  add_tiles 0 (DaPai Fa);
  add_tiles 0 (DaPai Bai);

  tiles

(**[shuffle] shuffles the tiles, used at initialization*)
let shuffle tiles =
  let rec shuffle_helper n =
    (*done shuffling*)
    if n = 0 then tiles
    else
      let i = Random.int n in
      let temp = tiles.(n) in
      tiles.(n) <- tiles.(i);
      tiles.(i) <- temp;
      shuffle_helper (n - 1)
  in
  shuffle_helper 135

type player = {
  index : int;
  mutable money : int;
  mutable hidden : tile option array;
  mutable exposed : (group * int) list;
}
(** RI: hidden is size 14, exposed is size 4, index is from 0-3*)

let tiles_arr = shuffle (init_tiles ())
let discarded = ref [ { num = 3110; tao = Tong } ]
let make_tile num tao = { num; tao }

let make_group = function
  | "Shun" -> Shun
  | "San" -> San
  | "Si" -> Si
  | _ -> failwith "Invalid string passed to make_group fxn"

let group_to_string (g : group) : string =
  match g with
  | Shun -> "ShunZi"
  | San -> "KeZi"
  | Si -> "Gang"

(* Edge case: DaPai -> no number in front, list of length 1 created *)
let string_to_tile str =
  try
    let t = Str.split (Str.regexp " ") str in
    (* list of number, followed by tile type *)
    if List.length t = 2 then (* normal tile (not Da Pai) *)
      let n = int_of_string (List.hd t) in
      match List.hd (List.tl t) with
      (* gets the tile type *)
      | "Tong" -> { num = n; tao = Tong }
      | "Wan" -> { num = n; tao = Wan }
      | "Tiao" -> { num = n; tao = Tiao }
      | _ -> failwith "Invalid string passed in, cannot convert to Tile type!"
    else if List.length t = 1 then (* Da Pai *)
      match List.hd t with
      | "Dong" -> { num = 0; tao = DaPai Dong }
      | "Nan" -> { num = 0; tao = DaPai Nan }
      | "Xi" -> { num = 0; tao = DaPai Xi }
      | "Bei" -> { num = 0; tao = DaPai Bei }
      | "Zhong" -> { num = 0; tao = DaPai Zhong }
      | "Fa" -> { num = 0; tao = DaPai Fa }
      | "Bai" -> { num = 0; tao = DaPai Bai }
      | _ -> failwith "Invalid string passed in, cannot convert to Tile type!"
    else failwith "Invalid string passed in, cannot convert to Tile type!"
  with Failure e when e = "int_of_string" ->
    failwith (str ^ " cannot be converted to Tile type: Invalid integer.")

let tile_to_string (t : tile) : string =
  Printf.sprintf "%s%s"
    (if t.num = 0 then "" else string_of_int t.num ^ " ")
    (match t.tao with
    | Tong -> "Tong"
    | Wan -> "Wan"
    | Tiao -> "Tiao"
    | DaPai dp -> (
        match dp with
        | Dong -> "Dong"
        | Nan -> "Nan"
        | Xi -> "Xi"
        | Bei -> "Bei"
        | Zhong -> "Zhong"
        | Fa -> "Fa"
        | Bai -> "Bai"))

exception InvalidTile of string
exception NoTileLeft

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
  | Fake
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

let shuffle tiles =
  let rec shuffle_helper n =
    (*swaps tiles at random numbers *)
    if n = 0 then ()
    else
      let i = Random.int n in
      let temp = tiles.(n) in
      tiles.(n) <- tiles.(i);
      tiles.(i) <- temp;
      shuffle_helper (n - 1)
  in
  shuffle_helper 135

(*blank tiles_arr to be initialized soon*)
let tiles_arr = ref [||]


let reset_tiles () = 
  tiles_arr := [||];
  curr_index :=  0

(**[init_tiles] initializes 136 tiles in a given order*)
let init_tiles () =
  (*makes "blank tiles"*)
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

  (*sets the tiles to be a "global" access variable*)
  tiles_arr := tiles;
  tiles

let discarded = ref [ { num = 3110; tao = Tong } ]

let make_tile num tao =
  if num < 0 || num > 9 then raise (InvalidTile "Invalid number for tile")
  else if tao = Tong && num = 0 then
    raise (InvalidTile "Tong cannot have num = 0")
  else if tao = Wan && num = 0 then
    raise (InvalidTile "Wan cannot have num = 0")
  else if tao = Tiao && num = 0 then
    raise (InvalidTile "Tiao cannot have num = 0")
  else { num; tao }

let make_group = function
  | "Shun" -> Shun
  | "San" -> San
  | "Si" -> Si
  | _ -> raise (InvalidTile "Invalid string passed to make_group fxn")

let group_to_string (g : group) : string =
  match g with
  | Shun -> "ShunZi"
  | San -> "KeZi"
  | Si -> "Gang"

let string_to_suit str =
  match str with
  | "Tong" -> Tong
  | "Wan" -> Wan
  | "Tiao" -> Tiao
  | "Dong" -> DaPai Dong
  | "Nan" -> DaPai Nan
  | "Xi" -> DaPai Xi
  | "Bei" -> DaPai Bei
  | "Zhong" -> DaPai Zhong
  | "Fa" -> DaPai Fa
  | "Bai" -> DaPai Bai
  | "Fake" -> Fake
  | _ -> raise (InvalidTile (str ^ " is not a valid suit string"))

let string_to_tile str =
  try
    let t = Str.split (Str.regexp " ") str in
    if List.length t = 2 then
      let n = int_of_string (List.hd t) in
      let tao = string_to_suit (List.hd (List.tl t)) in
      if
        n < 1 || n > 9
        ||
        match tao with
        | DaPai _ -> true
        | _ -> false
      then
        raise
          (InvalidTile
             (str ^ " is an invalid string, cannot convert to Tile type!"))
      else { num = n; tao }
    else if List.length t = 1 then
      match string_to_suit (List.hd t) with
      | DaPai x -> { num = 0; tao = DaPai x }
      | _ -> raise (InvalidTile (str ^ " is not a valid DaPai string"))
    else
      raise
        (InvalidTile
           (str ^ "Invalid string passed in, cannot convert to Tile type!"))
  with Failure e when e = "int_of_string" ->
    raise
      (InvalidTile (str ^ " cannot be converted to Tile type: Invalid integer."))

let suit_to_string (tao : suit) : string =
  match tao with
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
      | Bai -> "Bai")
  | Fake -> "Fake"

let tile_to_string (t : tile) : string =
  Printf.sprintf "%s%s"
    (if t.num = 0 then "" else string_of_int t.num ^ " ")
    (suit_to_string t.tao)

(* May the lord have mercy on our souls *)
let suit_prio = function
  | Tong -> 0
  | Wan -> 1
  | Tiao -> 2
  | DaPai dp -> (
      match dp with
      | Dong -> 3
      | Nan -> 4
      | Xi -> 5
      | Bei -> 6
      | Zhong -> 7
      | Fa -> 8
      | Bai -> 9)
  | Fake -> 3110
(* used to sort tiles by suit, then by number *)

let compare_tile t1 t2 =
  let suit_cmp = suit_prio (get_tao t1) - suit_prio (get_tao t2) in
  if suit_cmp <> 0 then suit_cmp else get_num t1 - get_num t2
(* used to sort tiles by number, then by suit *)

let fake_tile = { num = 3110; tao = Fake }

let tile_to_key (t : tile) : string =
  match t.tao with
  | Tong -> Printf.sprintf "%dtong" t.num
  | Wan -> Printf.sprintf "%dwan" t.num
  | Tiao -> Printf.sprintf "%dtiao" t.num
  | DaPai dp -> (
      match dp with
      | Dong -> "dong"
      | Nan -> "nan"
      | Xi -> "xi"
      | Bei -> "bei"
      | Zhong -> "zhong"
      | Fa -> "fa"
      | Bai -> "bai")
  | Fake -> "fake"

let tile_list_to_keys (tiles : tile list) : string list =
  List.sort compare_tile tiles |> List.map tile_to_key

open Mahjong

(*Ordered tile array: 一T * 4, 二T * 4, ... -> Wan -> Tiao -> 东南西北中發白*)
let tilesInOrder = Tile.init_tiles ()

(*Prints a tile array*)
let printDeck tilearr =
  Array.iter (fun tile -> print_endline (Tile.tile_to_string tile)) tilearr

let test_tiles =
  [
    (Tile.string_to_tile "1 Tong", "Tong", 1, "1 Tong");
    (Tile.string_to_tile "9 Tong", "Tong", 9, "9 Tong");
    (Tile.string_to_tile "2 Wan", "Wan", 2, "2 Wan");
    (Tile.string_to_tile "3 Tiao", "Tiao", 3, "3 Tiao");
    (Tile.string_to_tile "Dong", "Dong", 0, "Dong");
    (Tile.string_to_tile "Zhong", "Zhong", 0, "Zhong");
  ]

let test_bad_tiles =
  [ "10 Tong"; "0 Wan"; "Tiao"; "0 Nan"; "2 Zhong"; "6 Bei"; "0 Fa"; "Dragon" ]

let players_string =
  [ ("Caedy", 1); ("Albert", 2); ("Eleanor", 3); ("Jess", 4) ]

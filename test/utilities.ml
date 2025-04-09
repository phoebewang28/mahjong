open Mahjong

(*Ordered tile array: 一T * 4, 二T * 4, ... -> Wan -> Tiao -> 东南西北中發白*)
let tilesInOrder = Tile.init_tiles ()

(*Prints a tile array*)
let printDeck tilearr =
  Array.iter (fun tile -> print_endline (Tile.tile_to_string tile)) tilearr

let test_tiles =
  [
    (Tile.string_to_tile "1 Tong", "Tong", 1);
    (Tile.string_to_tile "2 Wan", "Wan", 2);
    (Tile.string_to_tile "3 Tiao", "Tiao", 3);
    (Tile.string_to_tile "Dong", "Dong", 0);
    (Tile.string_to_tile "Zhong", "Zhong", 0);
  ]

let test_bad_tiles =
  [ "10 Tong"; "0 Wan"; "Tiao"; "0 Dong"; "2 Zhong"; "Dragon" ]

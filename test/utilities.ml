open Mahjong
open QCheck

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

(* let test_hand_has_peng_chi = Hidden_hand.make_hidden_hand [
   Tile.string_to_tile "1 Tong"; Tile.string_to_tile "1 Tong";
   Tile.string_to_tile "1 Tong"; Tile.string_to_tile "2 Tong";
   Tile.string_to_tile "3 Tong"; Tile.string_to_tile "7 Wan";
   Tile.string_to_tile "8 Wan"; Tile.string_to_tile "9 Wan"; Tile.string_to_tile
   "Nan"; Tile.string_to_tile "Xi"; Tile.string_to_tile "Xi";
   Tile.string_to_tile "Bei"; Tile.string_to_tile "Bei"; ]

   let test_hand_no_peng_chi = Hidden_hand.make_hidden_hand [
   Tile.string_to_tile "1 Tong"; Tile.string_to_tile "2 Wan";
   Tile.string_to_tile "3 Tiao"; Tile.string_to_tile "3 Tong";
   Tile.string_to_tile "3 Tong"; Tile.string_to_tile "Dong"; Tile.string_to_tile
   "Dong"; Tile.string_to_tile "Nan"; Tile.string_to_tile "Nan";
   Tile.string_to_tile "Xi"; Tile.string_to_tile "Xi"; Tile.string_to_tile
   "Bei"; Tile.string_to_tile "Bei"; ] *)

let test_bad_tiles =
  [
    "0 Tong";
    "0 Wan";
    "0 Tiao";
    "10 Tong";
    "0 Wan";
    "Tiao";
    "0 Nan";
    "2 Zhong";
    "6 Bei";
    "0 Fa";
    "Dragon";
  ]

let players_string = [ ("Caedy", 1); ("Albert", 2); ("Elinor", 3); ("Jess", 4) ]
let player = Player.create "test" 0
let test_player hh eh = Player.make_player "test" 0 10 hh eh

(* use for qcheck *)
let arbitrary_tile =
  make
    (Gen.oneofl
       (List.map
          (fun (x, y) -> Tile.make_tile y (Tile.string_to_suit x))
          (List.map (fun x -> ("Tong", x)) [ 1; 2; 3; 4; 5; 6; 7; 8; 9 ]
          @ List.map (fun x -> ("Wan", x)) [ 1; 2; 3; 4; 5; 6; 7; 8; 9 ]
          @ List.map (fun x -> ("Tiao", x)) [ 1; 2; 3; 4; 5; 6; 7; 8; 9 ]
          @ List.map
              (fun x -> (x, 0))
              [ "Dong"; "Nan"; "Xi"; "Bei"; "Zhong"; "Fa"; "Bai" ])))

let hh1_tiles =
  [
    Tile.string_to_tile "1 Wan";
    Tile.string_to_tile "1 Wan";
    Tile.string_to_tile "2 Wan";
    Tile.string_to_tile "3 Wan";
    Tile.string_to_tile "4 Wan";
    Tile.string_to_tile "5 Wan";
    Tile.string_to_tile "6 Wan";
    Tile.string_to_tile "7 Wan";
    Tile.string_to_tile "3 Tong";
    Tile.string_to_tile "4 Tong";
    Tile.string_to_tile "5 Tong";
    Tile.string_to_tile "7 Tiao";
    Tile.string_to_tile "7 Tiao";
    Tile.string_to_tile "7 Tiao";
  ]

let hh2_tiles =
  [
    Tile.string_to_tile "1 Wan";
    Tile.string_to_tile "2 Wan";
    Tile.string_to_tile "2 Wan";
    Tile.string_to_tile "2 Wan";
    Tile.string_to_tile "3 Wan";
    Tile.string_to_tile "4 Wan";
    Tile.string_to_tile "4 Wan";
    Tile.string_to_tile "4 Wan";
    Tile.string_to_tile "7 Wan";
    Tile.string_to_tile "7 Wan";
    Tile.string_to_tile "7 Wan";
    Tile.string_to_tile "8 Wan";
    Tile.string_to_tile "8 Wan";
    Tile.string_to_tile "8 Wan";
  ]

let hh3_tiles =
  [
    Tile.string_to_tile "1 Tiao";
    Tile.string_to_tile "1 Tiao";
    Tile.string_to_tile "1 Tiao";
    Tile.string_to_tile "3 Tiao";
    Tile.string_to_tile "3 Tiao";
    Tile.string_to_tile "3 Tiao";
    Tile.string_to_tile "3 Tiao";
    Tile.string_to_tile "4 Tiao";
    Tile.string_to_tile "5 Tiao";
    Tile.string_to_tile "7 Tiao";
    Tile.string_to_tile "7 Tiao";
    Tile.string_to_tile "7 Tiao";
    Tile.string_to_tile "8 Tiao";
    Tile.string_to_tile "9 Tiao";
  ]

let hh4_tiles =
  [
    Tile.string_to_tile "1 Tiao";
    Tile.string_to_tile "1 Tiao";
    Tile.string_to_tile "1 Tiao";
    Tile.string_to_tile "1 Tiao";
    Tile.string_to_tile "2 Tiao";
    Tile.string_to_tile "2 Tiao";
    Tile.string_to_tile "2 Tiao";
    Tile.string_to_tile "3 Tiao";
    Tile.string_to_tile "4 Tiao";
    Tile.string_to_tile "4 Tiao";
    Tile.string_to_tile "4 Tiao";
    Tile.string_to_tile "5 Tiao";
    Tile.string_to_tile "6 Tiao";
    Tile.string_to_tile "7 Tiao";
  ]

let hh5_tiles =
  [
    Tile.string_to_tile "3 Tong";
    Tile.string_to_tile "3 Tong";
    Tile.string_to_tile "3 Tong";
    Tile.string_to_tile "4 Tong";
    Tile.string_to_tile "4 Tong";
    Tile.string_to_tile "4 Tong";
    Tile.string_to_tile "4 Tong";
    Tile.string_to_tile "6 Tong";
    Tile.string_to_tile "8 Tong";
    Tile.string_to_tile "8 Tong";
    Tile.string_to_tile "9 Tong";
    Tile.string_to_tile "9 Tong";
    Tile.string_to_tile "9 Tong";
    Tile.string_to_tile "5 Tong";
  ]

let hh6_tiles =
  [
    Tile.string_to_tile "1 Tong";
    Tile.string_to_tile "1 Tong";
    Tile.string_to_tile "1 Tong";
    Tile.string_to_tile "3 Tong";
    Tile.string_to_tile "4 Tong";
    Tile.string_to_tile "4 Tong";
    Tile.string_to_tile "5 Tong";
    Tile.string_to_tile "5 Tong";
    Tile.string_to_tile "6 Tong";
    Tile.string_to_tile "7 Tong";
    Tile.string_to_tile "7 Tong";
    Tile.string_to_tile "7 Tong";
    Tile.string_to_tile "9 Tong";
    Tile.string_to_tile "9 Tong";
  ]

let hh7_tiles =
  [
    Tile.string_to_tile "5 Tong";
    Tile.string_to_tile "5 Tong";
    Tile.string_to_tile "5 Tong";
    Tile.string_to_tile "6 Tong";
    Tile.string_to_tile "7 Tong";
    Tile.string_to_tile "Zhong";
    Tile.string_to_tile "Zhong";
    Tile.string_to_tile "Zhong";
    Tile.string_to_tile "Fa";
    Tile.string_to_tile "Fa";
    Tile.string_to_tile "Fa";
    Tile.string_to_tile "Bai";
    Tile.string_to_tile "Bai";
    Tile.string_to_tile "Bai";
  ]

let hh8_tiles =
  [
    Tile.string_to_tile "3 Wan";
    Tile.string_to_tile "4 Wan";
    Tile.string_to_tile "4 Wan";
    Tile.string_to_tile "4 Wan";
    Tile.string_to_tile "4 Wan";
    Tile.string_to_tile "5 Wan";
    Tile.string_to_tile "5 Wan";
    Tile.string_to_tile "6 Wan";
    Tile.string_to_tile "6 Wan";
    Tile.string_to_tile "6 Wan";
    Tile.string_to_tile "6 Wan";
    Tile.string_to_tile "7 Wan";
    Tile.string_to_tile "9 Wan";
    Tile.string_to_tile "9 Wan";
  ]

let hh9_tiles =
  [
    Tile.string_to_tile "1 Tong";
    Tile.string_to_tile "1 Tong";
    Tile.string_to_tile "2 Tong";
    Tile.string_to_tile "3 Tong";
    Tile.string_to_tile "4 Tong";
    Tile.string_to_tile "4 Tong";
    Tile.string_to_tile "5 Tong";
    Tile.string_to_tile "5 Tong";
    Tile.string_to_tile "6 Tong";
    Tile.string_to_tile "7 Tong";
    Tile.string_to_tile "7 Tong";
    Tile.string_to_tile "7 Tong";
    Tile.string_to_tile "9 Tong";
    Tile.string_to_tile "9 Tong";
  ]

let pinghu_hand =
  [
    Tile.string_to_tile "1 Wan";
    Tile.string_to_tile "2 Wan";
    Tile.string_to_tile "3 Wan";
    Tile.string_to_tile "4 Wan";
    Tile.string_to_tile "5 Wan";
    Tile.string_to_tile "6 Wan";
    Tile.string_to_tile "7 Wan";
    Tile.string_to_tile "8 Wan";
    Tile.string_to_tile "9 Wan";
    Tile.string_to_tile "2 Tiao";
    Tile.string_to_tile "3 Tiao";
    Tile.string_to_tile "4 Tiao";
    Tile.string_to_tile "5 Tong";
    Tile.string_to_tile "5 Tong";
  ]

let dasixi_hand1 =
  [
    Tile.string_to_tile "Dong";
    Tile.string_to_tile "Dong";
    Tile.string_to_tile "Dong";
    Tile.string_to_tile "Nan";
    Tile.string_to_tile "Nan";
    Tile.string_to_tile "Nan";
    Tile.string_to_tile "Xi";
    Tile.string_to_tile "Xi";
    Tile.string_to_tile "Xi";
    Tile.string_to_tile "Bei";
    Tile.string_to_tile "Bei";
    Tile.string_to_tile "Bei";
    Tile.string_to_tile "Zhong";
    Tile.string_to_tile "Zhong";
    ]

let dasixi_hand2 =
  [
    Tile.string_to_tile "Dong";
    Tile.string_to_tile "Dong";
    Tile.string_to_tile "Dong";
    Tile.string_to_tile "Nan";
    Tile.string_to_tile "Nan";
    Tile.string_to_tile "Nan";
    Tile.string_to_tile "Xi";
    Tile.string_to_tile "Xi";
    Tile.string_to_tile "Xi";
    Tile.string_to_tile "Bei";
    Tile.string_to_tile "Bei";
    Tile.string_to_tile "Zhong";
    Tile.string_to_tile "Zhong";
    Tile.string_to_tile "Zhong";
    ]

let dasanyuan_hand1 =
  [
    Tile.string_to_tile "1 Wan";
    Tile.string_to_tile "2 Wan";
    Tile.string_to_tile "3 Wan";
    Tile.string_to_tile "4 Wan";
    Tile.string_to_tile "4 Wan";
    Tile.string_to_tile "Zhong";
    Tile.string_to_tile "Zhong";
    Tile.string_to_tile "Zhong";
    Tile.string_to_tile "Fa";
    Tile.string_to_tile "Fa";
    Tile.string_to_tile "Fa";
    Tile.string_to_tile "Bai";
    Tile.string_to_tile "Bai";
    Tile.string_to_tile "Bai";
  ]

let dasanyuan_hand2 =
  [
    Tile.string_to_tile "1 Wan";
    Tile.string_to_tile "2 Wan";
    Tile.string_to_tile "3 Wan";
    Tile.string_to_tile "Zhong";
    Tile.string_to_tile "Zhong";
    Tile.string_to_tile "Zhong";
    Tile.string_to_tile "Fa";
    Tile.string_to_tile "Fa";
    Tile.string_to_tile "Fa";
    Tile.string_to_tile "Bai";
    Tile.string_to_tile "Bai";
    Tile.string_to_tile "Zhong";
    Tile.string_to_tile "Zhong";
  ]

let lvyise_hand1 =
  [
    Tile.string_to_tile "2 Tiao";
    Tile.string_to_tile "3 Tiao";
    Tile.string_to_tile "4 Tiao";
    Tile.string_to_tile "4 Tiao";
    Tile.string_to_tile "4 Tiao";
    Tile.string_to_tile "4 Tiao";
    Tile.string_to_tile "6 Tiao";
    Tile.string_to_tile "6 Tiao";
    Tile.string_to_tile "6 Tiao";
    Tile.string_to_tile "8 Tiao";
    Tile.string_to_tile "8 Tiao";
    Tile.string_to_tile "Fa";
    Tile.string_to_tile "Fa";
    Tile.string_to_tile "Fa";
    ]
let lvyise_hand2 =
  [
    Tile.string_to_tile "1 Tiao";
    Tile.string_to_tile "2 Tiao";
    Tile.string_to_tile "3 Tiao";
    Tile.string_to_tile "4 Tiao";
    Tile.string_to_tile "5 Tiao";
    Tile.string_to_tile "6 Tiao";
    Tile.string_to_tile "6 Tiao";
    Tile.string_to_tile "6 Tiao";
    Tile.string_to_tile "6 Tiao";
    Tile.string_to_tile "8 Tiao";
    Tile.string_to_tile "8 Tiao";
    Tile.string_to_tile "Fa";
    Tile.string_to_tile "Fa";
    Tile.string_to_tile "Fa";
    ]
let qidui_hand1 =
  [
    Tile.string_to_tile "1 Wan";
    Tile.string_to_tile "1 Wan";
    Tile.string_to_tile "5 Wan";
    Tile.string_to_tile "5 Wan";
    Tile.string_to_tile "9 Wan";
    Tile.string_to_tile "9 Wan";
    Tile.string_to_tile "2 Tong";
    Tile.string_to_tile "2 Tong";
    Tile.string_to_tile "3 Tong";
    Tile.string_to_tile "3 Tong";
    Tile.string_to_tile "8 Tiao";
    Tile.string_to_tile "8 Tiao";
    Tile.string_to_tile "Fa";
    Tile.string_to_tile "Fa";
    ]
let qidui_hand2 =
  [
    Tile.string_to_tile "1 Wan";
    Tile.string_to_tile "1 Wan";
    Tile.string_to_tile "2 Wan";
    Tile.string_to_tile "2 Wan";
    Tile.string_to_tile "3 Wan";
    Tile.string_to_tile "3 Wan";
    Tile.string_to_tile "2 Tong";
    Tile.string_to_tile "2 Tong";
    Tile.string_to_tile "2 Tong";
    Tile.string_to_tile "8 Tiao";
    Tile.string_to_tile "8 Tiao";
    Tile.string_to_tile "8 Tiao";
    Tile.string_to_tile "Fa";
    Tile.string_to_tile "Fa";
    ]

let jiulianbaodeng_hand1 =
  [
    Tile.string_to_tile "1 Wan";
    Tile.string_to_tile "1 Wan";
    Tile.string_to_tile "1 Wan";
    Tile.string_to_tile "1 Wan";
    Tile.string_to_tile "2 Wan";
    Tile.string_to_tile "3 Wan";
    Tile.string_to_tile "4 Wan";
    Tile.string_to_tile "5 Wan";
    Tile.string_to_tile "6 Wan";
    Tile.string_to_tile "7 Wan";
    Tile.string_to_tile "8 Wan";
    Tile.string_to_tile "9 Wan";
    Tile.string_to_tile "9 Wan";
    Tile.string_to_tile "9 Wan";
    ]
let jiulianbaodeng_hand2 =
  [
    Tile.string_to_tile "1 Wan";
    Tile.string_to_tile "1 Wan";
    Tile.string_to_tile "1 Wan";
    Tile.string_to_tile "1 Wan";
    Tile.string_to_tile "2 Wan";
    Tile.string_to_tile "3 Wan";
    Tile.string_to_tile "4 Wan";
    Tile.string_to_tile "5 Wan";
    Tile.string_to_tile "6 Wan";
    Tile.string_to_tile "7 Wan";
    Tile.string_to_tile "7 Wan";
    Tile.string_to_tile "9 Wan";
    Tile.string_to_tile "9 Wan";
    Tile.string_to_tile "9 Wan";
    ]

let duiduihu_hand1 =
  [
    Tile.string_to_tile "5 Wan";
    Tile.string_to_tile "5 Wan";
    Tile.string_to_tile "5 Wan";
    Tile.string_to_tile "8 Wan";
    Tile.string_to_tile "8 Wan";
    Tile.string_to_tile "8 Wan";
    Tile.string_to_tile "6 Tong";
    Tile.string_to_tile "6 Tong";
    Tile.string_to_tile "6 Tong";
    Tile.string_to_tile "8 Tong";
    Tile.string_to_tile "8 Tong";
    Tile.string_to_tile "8 Tong";
    Tile.string_to_tile "Bei";
    Tile.string_to_tile "Bei";
    ]
let duiduihu_hand2 =
  [
    Tile.string_to_tile "7 Wan";
    Tile.string_to_tile "8 Wan";
    Tile.string_to_tile "8 Wan";
    Tile.string_to_tile "8 Wan";
    Tile.string_to_tile "9 Wan";
    Tile.string_to_tile "8 Tong";
    Tile.string_to_tile "8 Tong";
    Tile.string_to_tile "8 Tong";
    Tile.string_to_tile "5 Tiao";
    Tile.string_to_tile "5 Tiao";
    Tile.string_to_tile "5 Tiao";
    Tile.string_to_tile "Bai";
    Tile.string_to_tile "Bai";
    Tile.string_to_tile "Bai";
    ]

let kankanhu_hand1 =
  [
    Tile.string_to_tile "5 Wan";
    Tile.string_to_tile "5 Wan";
    Tile.string_to_tile "5 Wan";
    Tile.string_to_tile "8 Wan";
    Tile.string_to_tile "8 Wan";
    Tile.string_to_tile "8 Wan";
    Tile.string_to_tile "6 Tong";
    Tile.string_to_tile "6 Tong";
    Tile.string_to_tile "6 Tong";
    Tile.string_to_tile "8 Tong";
    Tile.string_to_tile "8 Tong";
    Tile.string_to_tile "8 Tong";
    Tile.string_to_tile "Bei";
    Tile.string_to_tile "Bei";
    ]
let kankanhu_hand2 =
  [
    Tile.string_to_tile "7 Wan";
    Tile.string_to_tile "8 Wan";
    Tile.string_to_tile "8 Wan";
    Tile.string_to_tile "8 Wan";
    Tile.string_to_tile "9 Wan";
    Tile.string_to_tile "8 Tong";
    Tile.string_to_tile "8 Tong";
    Tile.string_to_tile "8 Tong";
    Tile.string_to_tile "5 Tiao";
    Tile.string_to_tile "5 Tiao";
    Tile.string_to_tile "5 Tiao";
    Tile.string_to_tile "Bai";
    Tile.string_to_tile "Bai";
    Tile.string_to_tile "Bai";
    ]

let qingyise_hand1 =
  [
    Tile.string_to_tile "1 Wan";
    Tile.string_to_tile "2 Wan";
    Tile.string_to_tile "2 Wan";
    Tile.string_to_tile "3 Wan";
    Tile.string_to_tile "3 Wan";
    Tile.string_to_tile "4 Wan";
    Tile.string_to_tile "4 Wan";
    Tile.string_to_tile "5 Wan";
    Tile.string_to_tile "5 Wan";
    Tile.string_to_tile "5 Wan";
    Tile.string_to_tile "6 Wan";
    Tile.string_to_tile "7 Wan";
    Tile.string_to_tile "8 Wan";
    Tile.string_to_tile "9 Wan";
  ]
let qingyise_hand2 =
  [
    Tile.string_to_tile "1 Tong";
    Tile.string_to_tile "1 Tong";
    Tile.string_to_tile "1 Tong";
    Tile.string_to_tile "3 Tong";
    Tile.string_to_tile "3 Tong";
    Tile.string_to_tile "3 Tong";
    Tile.string_to_tile "1 Tiao";
    Tile.string_to_tile "1 Tiao";
    Tile.string_to_tile "1 Tiao";
    Tile.string_to_tile "2 Tiao";
    Tile.string_to_tile "3 Tiao";
    Tile.string_to_tile "4 Tiao";
    Tile.string_to_tile "5 Tiao";
    Tile.string_to_tile "5 Tiao";
  ]

let ziyise_hand1 =
  [
    Tile.string_to_tile "Dong";
    Tile.string_to_tile "Dong";
    Tile.string_to_tile "Dong";
    Tile.string_to_tile "Nan";
    Tile.string_to_tile "Nan";
    Tile.string_to_tile "Bei";
    Tile.string_to_tile "Bei";
    Tile.string_to_tile "Bei";
    Tile.string_to_tile "Zhong";
    Tile.string_to_tile "Zhong";
    Tile.string_to_tile "Zhong";
    Tile.string_to_tile "Bai";
    Tile.string_to_tile "Bai";
    Tile.string_to_tile "Bai";
  ]
let ziyise_hand2 =
  [
    Tile.string_to_tile "1 Tong";
    Tile.string_to_tile "1 Tong";
    Tile.string_to_tile "1 Tong";
    Tile.string_to_tile "Nan";
    Tile.string_to_tile "Nan";
    Tile.string_to_tile "Bei";
    Tile.string_to_tile "Bei";
    Tile.string_to_tile "Bei";
    Tile.string_to_tile "Zhong";
    Tile.string_to_tile "Zhong";
    Tile.string_to_tile "Zhong";
    Tile.string_to_tile "Bai";
    Tile.string_to_tile "Bai";
    Tile.string_to_tile "Bai";
  ]
let hunyise_hand1 =
  [
    Tile.string_to_tile "1 Tong";
    Tile.string_to_tile "1 Tong";
    Tile.string_to_tile "1 Tong";
    Tile.string_to_tile "Nan";
    Tile.string_to_tile "Nan";
    Tile.string_to_tile "Bei";
    Tile.string_to_tile "Bei";
    Tile.string_to_tile "Bei";
    Tile.string_to_tile "Zhong";
    Tile.string_to_tile "Zhong";
    Tile.string_to_tile "Zhong";
    Tile.string_to_tile "Bai";
    Tile.string_to_tile "Bai";
    Tile.string_to_tile "Bai";
  ]
let hunyise_hand2 =
  [
    Tile.string_to_tile "1 Wan";
    Tile.string_to_tile "1 Wan";
    Tile.string_to_tile "1 Tong";
    Tile.string_to_tile "1 Tong";
    Tile.string_to_tile "1 Tong";
    Tile.string_to_tile "Bei";
    Tile.string_to_tile "Bei";
    Tile.string_to_tile "Bei";
    Tile.string_to_tile "Zhong";
    Tile.string_to_tile "Zhong";
    Tile.string_to_tile "Zhong";
    Tile.string_to_tile "Bai";
    Tile.string_to_tile "Bai";
    Tile.string_to_tile "Bai";
  ]
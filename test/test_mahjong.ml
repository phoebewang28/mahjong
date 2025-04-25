open OUnit2
open Mahjong
open Utilities
open QCheck

let tile_test fn expected_val print name =
  name >:: fun _ -> assert_equal fn expected_val ~printer:print

(**Tests for any exception of type InvalidTile, the other way was forcing me to
   have universal error messages.*)
let tile_test_raise name fn tile =
  name >:: fun _ ->
  try
    let _ = fn tile in
    assert_failure "Expected InvalidTile exception was not raised."
  with
  | Tile.InvalidTile _ -> ()
  | e -> raise e

(**Tests get_num function.*)
let tile_num_test tile expected_val =
  tile_test (Tile.get_num tile) expected_val string_of_int
    ("get_num test: " ^ Tile.tile_to_string tile)

(**Tests get_tao function.*)
let tile_suit_test tile (expected_val : string) =
  tile_test expected_val
    (Tile.suit_to_string (Tile.get_tao tile))
    (fun x -> x)
    ("get_suit test: " ^ Tile.tile_to_string tile)

(** [make_tile_test num suit] verifies the properties of a tile via qcheck. It
    checks if the tile's tao/suit matches [suit], and its number is [num].*)
let make_tile_test num suit =
  let tile = Tile.make_tile num suit in
  Tile.get_num tile = num && Tile.get_tao tile = suit

(**Tests for the [tile_to_string] function.*)
let tile_to_string_test tile expected_val =
  tile_test (Tile.tile_to_string tile) expected_val
    (fun x -> x)
    ("tile_to_string test: " ^ Tile.tile_to_string tile)

(**Tests for the [string_to_tile] function.*)
let string_to_tile_test str expected_tile =
  tile_test (Tile.string_to_tile str) expected_tile Tile.tile_to_string
    ("string_to_tile test: " ^ str)

(**Tests for the [compare_tiles] function.*)
let compare_tiles_test expected_val exp_bool tile1 tile2 =
  tile_test
    (Tile.compare_tile tile1 tile2 < expected_val)
    exp_bool string_of_bool
    ("compare_tiles test: " ^ Tile.tile_to_string tile1 ^ " vs "
   ^ Tile.tile_to_string tile2)

let draw_next_tile_test expected_tile player curr =
  "draw test " ^ Tile.tile_to_string expected_tile >:: fun _ ->
  Tile.curr_index := curr;
  let drawn = Player_choice.draw player in
  assert_equal expected_tile drawn ~printer:Tile.tile_to_string

let player_choice_tests =
  let player = Utilities.player in
  [ draw_next_tile_test (Tile.string_to_tile "1 Tong") player 0 ]
  @ [ draw_next_tile_test (Tile.string_to_tile "2 Tong") player 4 ]

(**Tests for the [Player.create] function.*)

let create_player_test p_name p_index =
  let player = Player.create p_name p_index in
  [
    ( p_name ^ " name test" >:: fun _ ->
      assert_equal (Player.get_name player) p_name ~printer:(fun x -> x) );
    ( p_name ^ " index test" >:: fun _ ->
      assert_equal (Player.get_index player) p_index ~printer:string_of_int );
    ( p_name ^ " money init test" >:: fun _ ->
      assert_equal (Player.get_money player) 1500 ~printer:string_of_int );
  ]

(**Tests for the [Player.winner] function.*)

let player_win_test (p : Player.player) (players : Player.player list) bet =
  let original_money = Hashtbl.create 4 in
  List.iter
    (fun a ->
      Hashtbl.replace original_money (Player.get_name a) (Player.get_money a))
    players;
  Hashtbl.iter
    (fun key value -> Printf.printf "%s: %d\n" key value)
    original_money;
  Player.winner p players bet;
  Player.get_name p ^ " wins out of "
  ^ String.concat ", " (List.map Player.get_name players)
  >:: fun _ ->
  assert_equal
    (Hashtbl.find original_money (Player.get_name p) + (3 * bet))
    (Player.get_money p) ~printer:string_of_int;

  (*to debug*)
  (* print_endline (string_of_int (Player.get_money p)); print_endline
     (string_of_int (Hashtbl.find original_money (Player.get_name p) + (3 *
     bet))); print_endline "-----"; List.iter (fun a -> print_endline
     (string_of_int (Hashtbl.find original_money (Player.get_name a) - bet));
     print_endline (string_of_int (Player.get_money a))) (List.filter (fun a ->
     a <> p) players); *)
  List.iter
    (fun a ->
      assert_equal
        (Hashtbl.find original_money (Player.get_name a) - bet)
        (Player.get_money a) ~printer:string_of_int)
    (List.filter (fun a -> a <> p) players)

let ocahmahjong_team = List.map (fun (a, b) -> Player.create a b) players_string

let player_tests =
  List.flatten
    (List.map (fun (a, b) -> create_player_test a b) Utilities.players_string)
  @ [ player_win_test (List.hd ocahmahjong_team) ocahmahjong_team 10 ]
(* @ [ player_win_test (List.hd ocahmahjong_team) ocahmahjong_team 10 ] *)

let tile_tests =
  (*Tests that will be applied on various selected tiles, refer to
    Utilities.ml*)
  List.map (fun (a, _, c, _) -> tile_num_test a c) Utilities.test_tiles
  @ List.map (fun (a, b, _, _) -> tile_suit_test a b) Utilities.test_tiles
  @ List.map
      (fun a ->
        tile_test_raise ("make tile fail test " ^ a) Tile.string_to_tile a)
      Utilities.test_bad_tiles
  @ List.mapi
      (fun i (a, _, _, d) ->
        if i < List.length Utilities.test_tiles - 1 then
          let b, _, _, e = List.nth Utilities.test_tiles (i + 1) in
          compare_tiles_test 0 true a b
        else tile_test "meh" "meh" (fun x -> x) "huh")
      Utilities.test_tiles
  @ List.map (fun (a, _, _, d) -> tile_to_string_test a d) Utilities.test_tiles
  @ List.map (fun (a, _, _, d) -> string_to_tile_test d a) Utilities.test_tiles
  @
  (*Tests that are moreso invariant checkers / one-time cases*)
  [
    tile_test
      (Array.length (Tile.init_tiles ()))
      136 string_of_int "test init_tiles length";
    compare_tiles_test 0 false
      (Tile.make_tile 0 (Tile.string_to_suit "Bei"))
      (Tile.make_tile 0 (Tile.string_to_suit "Bei"));
    compare_tiles_test 1 true
      (Tile.make_tile 0 (Tile.string_to_suit "Bei"))
      (Tile.make_tile 0 (Tile.string_to_suit "Bei"));
    (* qtest (make_tile_test 1) 100; *)
  ]

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

let complete_test name tiles expected =
  name >:: fun _ ->
  let hand = Hidden_hand.make_hidden_hand tiles in
  let p =
    Player.make_player "TestPlayer" 0 0 hand
      (Exposed_hand.empty_exposed_hand ())
  in

  Printf.printf "[DEBUG] Hand: %s\n"
    (String.concat " | "
       (List.map Tile.tile_to_string (Hidden_hand.get_tiles hand)));

  let hidden_tiles = Hidden_hand.get_tiles hand in
  let unique_tiles = List.sort_uniq Tile.compare_tile hidden_tiles in
  List.iter
    (fun t ->
      let count =
        List.length
          (List.filter (fun x -> Tile.compare_tile x t = 0) hidden_tiles)
      in
      if count >= 2 then
        Printf.printf "[DEBUG] Trying pair candidate: %s (x%d)\n"
          (Tile.tile_to_string t) count)
    unique_tiles;

  let result = Ying.complete p in

  Printf.printf "[DEBUG] Result: %b (Expected: %b)\n\n" result expected;

  assert_equal expected result ~printer:string_of_bool

let complete_test_list =
  [ complete_test "test1" hh1_tiles true ]
  @ [ complete_test "test2" hh2_tiles true ]
    (* Empty hand should not complete *)
  @ [ complete_test "test3" [] false ]
  (* Single tile should not complete *)
  @ [ complete_test "test4" [ Tile.string_to_tile "1 Wan" ] false ]
  @ [ complete_test "test5" hh3_tiles true ]
  @ [ complete_test "test6" hh4_tiles true ]
  (* Empty hand should not complete *)
  @ [ complete_test "test4" [ Tile.string_to_tile "1 Wan" ] false ]
  (* Single tile should not complete *)
  @ [ complete_test "test5" hh5_tiles true ]
  @ [ complete_test "test6" hh6_tiles true ]
  @ [ complete_test "test7" hh7_tiles true ]

let tests =
  "test suite" >::: tile_tests @ tile_tests @ player_tests @ complete_test_list

let _ = run_test_tt_main tests

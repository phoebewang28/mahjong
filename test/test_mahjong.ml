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

let tests =
  "test suite"
  >::: [ ("a trivial test" >:: fun _ -> assert_equal 0 0) ] @ tile_tests

let _ = run_test_tt_main tests

open OUnit2
open Mahjong
open Utilities
open QCheck

let tile_test fn expected_val print name =
  name >:: fun _ -> assert_equal fn expected_val ~printer:print

(**Tests for any exception of type [InvalidTile], the other way was forcing me
   to have universal error messages.*)
let tile_test_raise name fn tile =
  name >:: fun _ ->
  try
    let _ = fn tile in
    assert_failure "Expected InvalidTile exception was not raised."
  with
  | Tile.InvalidTile _ -> ()
  | e -> raise e

(**Tests [get_num] function.*)
let tile_num_test tile expected_val =
  tile_test (Tile.get_num tile) expected_val string_of_int
    ("get_num test: " ^ Tile.tile_to_string tile)

(**Tests for the [shuffle] function.*)
let shuffle_test name tiles =
  name >:: fun _ ->
  let original_tiles = Array.copy tiles in
  Tile.shuffle tiles;
  assert_equal
    (Array.sort Tile.compare_tile tiles;
     tiles)
    (Array.sort Tile.compare_tile original_tiles;
     original_tiles)
    ~printer:(fun arr ->
      String.concat ", " (Array.to_list (Array.map Tile.tile_to_string arr)))

(**Tests [get_tao] function.*)
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

let hh_size_test move player =
  match move with
  | "draw" ->
      "hidden hand size changes after draw test" >:: fun _ ->
      let size_before = Hidden_hand.get_size (Player.get_hidden player) in
      let _ = Player_choice.draw player in
      let size_after = Hidden_hand.get_size (Player.get_hidden player) in
      assert_equal (size_before + 1) size_after ~printer:string_of_int
  | "throw" ->
      "hidden hand size changes after throw test" >:: fun _ ->
      let size_before = Hidden_hand.get_size (Player.get_hidden player) in
      let _ = Player_choice.throw player 0 in
      let size_after = Hidden_hand.get_size (Player.get_hidden player) in
      assert_equal (size_before - 1) size_after ~printer:string_of_int
  | _ -> "test only draw and throw" >:: fun _ -> assert_equal 0 1

let draw_curr_ind_test player =
  "curr_index increments by 1 after draw" >:: fun _ ->
  let ind_before = !Tile.curr_index in
  let _ = Player_choice.draw player in
  let ind_after = !Tile.curr_index in
  assert_equal (ind_before + 1) ind_after ~printer:string_of_int

let throw_discarded_test player id =
  "the last discarded tile is the one just thrown" >:: fun _ ->
  let thrown = Player_choice.throw player id in
  let dis_after = List.hd !Tile.discarded in
  assert_equal thrown dis_after ~printer:Tile.tile_to_string

let chi_check_qcheck_test =
  QCheck_runner.to_ounit2_test
    (let arbitrary_hand =
       QCheck.list_of_size (QCheck.int_range 13 14).gen Utilities.arbitrary_tile
     in
     QCheck.Test.make ~count:100
       ~name:"tests chi_check with a randomly generated hand" arbitrary_hand
       (fun x ->
         let result =
           Player_choice.chi_check (Hidden_hand.make_hidden_hand x)
         in
         result))

let chi_peng_check_test move expected hand =
  Tile.discarded :=
    Tile.make_tile 1 (Tile.string_to_suit "Tong") :: !Tile.discarded;
  match move with
  | "chi" ->
      "tests if chi_check works properly " ^ string_of_bool expected
      >:: fun _ -> assert_equal (Player_choice.chi_check hand) expected
  | "peng" ->
      "tests if peng works properly " ^ string_of_bool expected >:: fun _ ->
      assert_equal (Player_choice.peng_check hand) expected
  | _ -> "use chi or peng only" >:: fun _ -> assert_equal 0 1

let chi_test player t1 t2 expected =
  "test chi function" ^ string_of_int t1 ^ string_of_int t2 >:: fun _ ->
  assert_equal (Player_choice.chi player t1 t2) expected

let peng_test player t1 t2 expected =
  "test peng function" >:: fun _ ->
  assert_equal (Player_choice.peng player t1 t2) expected

let player_choice_tests =
  let player = Utilities.player in
  [
    draw_next_tile_test (Tile.string_to_tile "1 Tong") player 0;
    draw_next_tile_test (Tile.string_to_tile "2 Tong") player 4;
  ]
  @ [ hh_size_test "draw" player ]
  @ [ hh_size_test "throw" player ]
  @ [ draw_curr_ind_test player ]
  @ [ throw_discarded_test player 0; throw_discarded_test player 1 ]
  @ [
      chi_peng_check_test "chi" true
        (Hidden_hand.make_hidden_hand Utilities.hh9_tiles);
    ]
  @ [
      chi_peng_check_test "chi" false
        (Hidden_hand.make_hidden_hand Utilities.hh5_tiles);
    ]
  @ [
      chi_peng_check_test "peng" true
        (Hidden_hand.make_hidden_hand Utilities.hh6_tiles);
    ]
  @ [
      chi_peng_check_test "peng" false
        (Hidden_hand.make_hidden_hand Utilities.pinghu_hand);
    ]
  @ [
      chi_test
        (Utilities.test_player
           (Hidden_hand.make_hidden_hand Utilities.hh9_tiles)
           (Exposed_hand.empty_exposed_hand ()))
        2 3 true;
    ]
  @ [
      chi_test
        (Utilities.test_player
           (Hidden_hand.make_hidden_hand Utilities.hh7_tiles)
           (Exposed_hand.empty_exposed_hand ()))
        0 1 false;
    ]
  @ [
      peng_test
        (Utilities.test_player
           (Hidden_hand.make_hidden_hand Utilities.hh6_tiles)
           (Exposed_hand.empty_exposed_hand ()))
        0 1 true;
    ]
  @ [
      peng_test
        (Utilities.test_player
           (Hidden_hand.make_hidden_hand Utilities.hh6_tiles)
           (Exposed_hand.empty_exposed_hand ()))
        10 11 false;
    ]

(**Tests for the [Player.create] function. These check if the name, index, and
   money accessor functions work properly.*)
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
    shuffle_test "shuffle test 1" (Tile.init_tiles ());
    shuffle_test "shuffle test 2" (Tile.init_tiles ());
    shuffle_test "shuffle test 3" (Tile.init_tiles ());
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

let pinghu_test name tiles expected =
  name >:: fun _ ->
  let hand = Hidden_hand.make_hidden_hand tiles in
  let p =
    Player.make_player "TestPlayer" 0 0 hand
      (Exposed_hand.empty_exposed_hand ())
  in

  Printf.printf "[DEBUG][Pinghu] Hand: %s\n"
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
        Printf.printf "[DEBUG][Pinghu] Trying pair candidate: %s (x%d)\n"
          (Tile.tile_to_string t) count)
    unique_tiles;

  let result = Ying.pinghu p in

  Printf.printf "[DEBUG][Pinghu] Result: %b (Expected: %b)\n\n" result expected;

  assert_equal expected result ~printer:string_of_bool

let pinghu_test_list = [ pinghu_test "pinghu1" Utilities.pinghu_hand true ]
let jiulianbaodeng_test name tiles expected =
  name >:: fun _ ->
  let hand = Hidden_hand.make_hidden_hand tiles in
  let p =
    Player.make_player "TestPlayer" 0 0 hand
      (Exposed_hand.empty_exposed_hand ())
  in

  Printf.printf "[DEBUG][jiulianbaodeng] Hand: %s\n"
    (String.concat " | "
       (List.map Tile.tile_to_string (Hidden_hand.get_tiles hand)));

  let result = Ying.jiulianbaodeng p in

  Printf.printf "[DEBUG][jiulianbaodeng] Result: %b (Expected: %b)\n\n" result expected;

  assert_equal expected result ~printer:string_of_bool

let jiulianbaodeng_test_list =
  [ jiulianbaodeng_test "jiulianbaodeng" Utilities.jiulianbaodeng_hand1 true ]
  @ [ jiulianbaodeng_test "jiulianbaodeng2" Utilities.jiulianbaodeng_hand2 false ]

let qidui_test name tiles expected =
  name >:: fun _ ->
  let hand = Hidden_hand.make_hidden_hand tiles in
  let p =
    Player.make_player "TestPlayer" 0 0 hand
      (Exposed_hand.empty_exposed_hand ())
  in

  Printf.printf "[DEBUG][qidui] Hand: %s\n"
    (String.concat " | "
       (List.map Tile.tile_to_string (Hidden_hand.get_tiles hand)));

  let result = Ying.qidui p in

  Printf.printf "[DEBUG][qidui] Result: %b (Expected: %b)\n\n" result expected;

  assert_equal expected result ~printer:string_of_bool

let qidui_test_list =
  [ qidui_test "qidui" Utilities.qidui_hand1 true ]
  @ [ qidui_test "qidui2" Utilities.qidui_hand2 false ]

let lvyise_test name tiles expected =
  name >:: fun _ ->
  let hand = Hidden_hand.make_hidden_hand tiles in
  let p =
    Player.make_player "TestPlayer" 0 0 hand
      (Exposed_hand.empty_exposed_hand ())
  in

  Printf.printf "[DEBUG][Lvyise] Hand: %s\n"
    (String.concat " | "
       (List.map Tile.tile_to_string (Hidden_hand.get_tiles hand)));

  let result = Ying.lvyise p in

  Printf.printf "[DEBUG][Lvyise] Result: %b (Expected: %b)\n\n" result expected;

  assert_equal expected result ~printer:string_of_bool

let lvyise_test_list =
  [ lvyise_test "lvyise" Utilities.lvyise_hand1 true ]
  @ [ lvyise_test "lvyise2" Utilities.lvyise_hand2 false ]

let dasixi_test name tiles expected =
  name >:: fun _ ->
  let hand = Hidden_hand.make_hidden_hand tiles in
  let p =
    Player.make_player "TestPlayer" 0 0 hand
      (Exposed_hand.empty_exposed_hand ())
  in

  Printf.printf "[DEBUG][dasixi] Hand: %s\n"
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
        Printf.printf "[DEBUG][dasixi] Trying pair candidate: %s (x%d)\n"
          (Tile.tile_to_string t) count)
    unique_tiles;

  let result = Ying.dasixi p in

  Printf.printf "[DEBUG][dasixi] Result: %b (Expected: %b)\n\n" result expected;

  assert_equal expected result ~printer:string_of_bool
let dasixi_test_list =
  [ dasixi_test "dasixi" Utilities.dasixi_hand1 true ]
  @ [ dasixi_test "dasixi2" Utilities.dasixi_hand2 false ]


  let dasanyuan_test name tiles expected =
    name >:: fun _ ->
    let hand = Hidden_hand.make_hidden_hand tiles in
    let p =
      Player.make_player "TestPlayer" 0 0 hand
        (Exposed_hand.empty_exposed_hand ())
    in
  
    Printf.printf "[DEBUG][dasanyuan] Hand: %s\n"
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
          Printf.printf "[DEBUG][dasanyuan] Trying pair candidate: %s (x%d)\n"
            (Tile.tile_to_string t) count)
      unique_tiles;
  
    let result = Ying.dasanyuan p in
  
    Printf.printf "[DEBUG][dasanyuan] Result: %b (Expected: %b)\n\n" result expected;
  
    assert_equal expected result ~printer:string_of_bool
  let dasanyuan_test_list =
    [ dasanyuan_test "dasanyuan" Utilities.dasanyuan_hand1 true ]
    @ [ dasanyuan_test "dasanyuan2" Utilities.dasanyuan_hand2 false ]
  
(* List of complete tests *)

let complete_test_list =
  [ complete_test "test1" Utilities.hh1_tiles true ]
  @ [ complete_test "test2" Utilities.hh2_tiles true ]
    (* Empty hand should not complete *)
  @ [ complete_test "test3" [] false ]
  (* Single tile should not complete *)
  @ [ complete_test "test4" [ Tile.string_to_tile "1 Wan" ] false ]
  @ [ complete_test "test5" Utilities.hh3_tiles true ]
  @ [ complete_test "test6" Utilities.hh4_tiles true ]
  (* Empty hand should not complete *)
  @ [ complete_test "test4" [ Tile.string_to_tile "1 Wan" ] false ]
  (* Single tile should not complete *)
  @ [ complete_test "test5" Utilities.hh5_tiles true ]
  @ [ complete_test "test6" Utilities.hh6_tiles true ]
  @ [ complete_test "test7" Utilities.hh7_tiles true ]
  @ [ complete_test "test8" Utilities.hh8_tiles true ]

(*initialized some variables to be used in the exposed hand tests*)
let suit = Tile.string_to_suit "Tong"
let tile1 = Tile.make_tile 1 suit
let tile2 = Tile.make_tile 2 suit
let tile3 = Tile.make_tile 3 suit
let tile4 = Tile.make_tile 4 suit
let tile5 = Tile.make_tile 5 suit

(** [eh_chi_test tiles] is a test helper function that applies peng to [tiles]
    and checks if the resulting exposed hand is as expected. *)
let eh_chi_test tiles =
  let hand = Exposed_hand.empty_exposed_hand () in
  "chi test" >:: fun _ ->
  List.iter (fun a -> Exposed_hand.chi a hand) tiles;
  (*tiles and groups are in reverse order*)
  let groups = List.rev !(Exposed_hand.get_hand hand) in
  List.iter2
    (fun tile group ->
      assert_equal
        (Printf.sprintf "%s ShunZi" (Tile.tile_to_string tile))
        (Tile.tile_to_string (snd group)
        ^ " "
        ^ Tile.group_to_string (fst group))
        ~printer:(fun x -> x))
    tiles groups

(** [eh_gang_test tiles] is a test helper function that applies ming_gang to
    [tiles] and checks if the resulting exposed hand is as expected. *)
let eh_gang_test tiles =
  let hand = Exposed_hand.empty_exposed_hand () in
  "gang test" >:: fun _ ->
  List.iter (fun a -> Exposed_hand.ming_gang a hand) tiles;
  (*tiles and groups are in reverse order*)
  let groups = List.rev !(Exposed_hand.get_hand hand) in
  List.iter2
    (fun tile group ->
      assert_equal
        (Printf.sprintf "%s Gang" (Tile.tile_to_string tile))
        (Tile.tile_to_string (snd group)
        ^ " "
        ^ Tile.group_to_string (fst group))
        ~printer:(fun x -> x))
    tiles groups

(** [eh_peng_test tiles] is a test helper function that applies peng to [tiles]
    and checks if the resulting exposed hand is as expected. *)
let eh_peng_test tiles =
  let hand = Exposed_hand.empty_exposed_hand () in
  "peng test" >:: fun _ ->
  List.iter (fun a -> Exposed_hand.peng a hand) tiles;
  (*tiles and groups are in reverse order*)
  let groups = List.rev !(Exposed_hand.get_hand hand) in
  List.iter2
    (fun tile group ->
      assert_equal
        (Printf.sprintf "%s KeZi" (Tile.tile_to_string tile))
        (Tile.tile_to_string (snd group)
        ^ " "
        ^ Tile.group_to_string (fst group))
        ~printer:(fun x -> x))
    tiles groups

(** [eh_get_tiles_test func tiles expected_tiles] is a test helper function that
    applies [func] (either gang, chi, or peng) to [tiles] and checks if the
    result matches [expected_tiles]. *)
let eh_get_tiles_test func tiles expected_tiles =
  let hand = Exposed_hand.empty_exposed_hand () in
  "get_tiles test" >:: fun _ ->
  List.iter (fun a -> func a hand) tiles;
  let actual_tiles = Exposed_hand.get_tiles hand in
  assert_equal
    (List.map Tile.tile_to_string expected_tiles)
    (List.map Tile.tile_to_string actual_tiles)
    ~printer:(String.concat ", ")

let exposed_hand_test =
  [
    ( "empty exposed hand test" >:: fun _ ->
      assert_equal
        (Exposed_hand.exposed_hand_to_string
           (Exposed_hand.empty_exposed_hand ()))
        ""
        ~printer:(fun x -> x) );
    eh_chi_test [ tile1 ];
    eh_chi_test [ tile1; tile2 ];
    eh_peng_test [ tile3 ];
    eh_peng_test [ tile3; tile4 ];
    eh_gang_test [ tile2 ];
    eh_gang_test [ tile4 ];
    eh_get_tiles_test Exposed_hand.chi [ tile1 ] [ tile1; tile2; tile3 ];
    eh_get_tiles_test Exposed_hand.peng [ tile1 ] [ tile1; tile1; tile1 ];
    eh_get_tiles_test Exposed_hand.chi [ tile1; tile2 ]
      [ tile2; tile3; tile4; tile1; tile2; tile3 ];
    eh_get_tiles_test Exposed_hand.peng [ tile3; tile4 ]
      [ tile4; tile4; tile4; tile3; tile3; tile3 ];
    eh_get_tiles_test Exposed_hand.ming_gang [ tile1 ]
      [ tile1; tile1; tile1; tile1 ];
    eh_get_tiles_test Exposed_hand.ming_gang [ tile3; tile4 ]
      [ tile4; tile4; tile4; tile4; tile3; tile3; tile3; tile3 ];
  ]

let tests =
  "test suite"
  >::: tile_tests @ player_tests @ player_choice_tests @ complete_test_list
       @ pinghu_test_list @ exposed_hand_test @ dasixi_test_list @ dasanyuan_test_list
       @ lvyise_test_list @ qidui_test_list

let _ = run_test_tt_main tests

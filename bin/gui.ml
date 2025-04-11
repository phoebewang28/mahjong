(* GUI version of main *)

open Raylib
open Raygui
open Mahjong
open Lwt

(* tiles might wanna be buttons, since we have to click to select it *)

type game_board = {
  player_lst : Player.player list;
  mutable cur_player : int; (* index of current player: 0,1,2,3 *)
  mutable player_hid : Hidden_hand.hidden_hand;
      (* hidden hand of CURRENT player *)
  mutable player_exp : Exposed_hand.exposed_hand;
  mutable discard : string; (*for now, just one*)
  mutable is_drawn : bool;
      (* state to track if drawn button is already clicked -> if true, will
         render draw; if false, will not *)
  mutable is_chi : bool;
  mutable is_peng : bool;
  mutable chi_fail : bool;
  mutable peng_fail : bool;
}

let window_width = 800
let window_height = 600

(** Center of the window *)
let center_x = window_width / 2

let center_y = window_height / 2

let draw_chi_fail () : unit =
  draw_text "Chi was unsuccessful!" 200 200 30 Color.white

let draw_peng_fail () : unit =
  draw_text "Peng was unsuccessful!" 200 200 30 Color.white

(* todo: prob try combining into a list *)

(* todo: game logic: must check if CAN chi first b4 rendering!! *)
let draw_chi_button p gb =
  let rect =
    Raylib.Rectangle.create
      (float_of_int window_width -. 300.)
      (float_of_int window_height -. 400.)
      100.0 70.0
  in
  (* note: button in raygui automatically accounts for user's mouse position &
     when clicked *)
  let is_chi_clicked = Raygui.button rect "Chi Tile" in
  if is_chi_clicked then
    if Player_choice.chi p then (
      gb.is_peng <- true;
      (* also disable other actions *)
      gb.is_chi <- true;
      gb.is_drawn <- true)
    else (
      gb.chi_fail <- true;
      (* i dont know how to do promise oof
      
      Lwt.async (fun () ->
          Lwt_unix.sleep 0.5 >>= fun () ->
          gb.chi_fail <- false;
          Lwt.return_unit)) *))

let draw_peng_button p gb =
  let rect =
    Raylib.Rectangle.create
      (float_of_int window_width -. 300.)
      (float_of_int window_height -. 300.)
      100.0 70.0
  in
  (* note: button in raygui automatically accounts for user's mouse position &
     when clicked *)
  let is_peng_clicked = Raygui.button rect "Peng Tile" in
  if is_peng_clicked then
    if Player_choice.chi p then (
      gb.is_peng <- true;
      (* also disable other actions *)
      gb.is_chi <- true;
      gb.is_drawn <- true)
    else draw_text "Chi was unsuccessful!" 200 200 30 Color.white

(** Effect: when player [p] clicks draw button, returns updated player hand *)
let draw_draw_button p gb =
  let rect =
    Raylib.Rectangle.create
      (float_of_int window_width -. 300.)
      (float_of_int window_height -. 200.)
      100.0 70.0
  in
  (* note: button in raygui automatically accounts for user's mouse position &
     when clicked *)
  let is_draw_clicked = Raygui.button rect "Draw Tile" in
  if is_draw_clicked then (* update player's hidden hand -> add card *)
    (
    Player_choice.draw p;
    gb.is_peng <- true;
    (* also disable other actions *)
    gb.is_chi <- true;
    gb.is_drawn <- true)

(** Effect: when player [p] clicks throw button, returns true if thrown is
    completed. *)
let draw_throw_button p : bool =
  let rect =
    Raylib.Rectangle.create
      (float_of_int window_width -. 300.)
      (float_of_int window_height -. 500.)
      100.0 70.0
  in
  (* note: button in raygui automatically accounts for user's mouse position &
     when clicked *)
  let is_throw_clicked = Raygui.button rect "Throw Tile" in
  if is_throw_clicked then (
    Player_choice.throw p;
    true)
  else false

(** [make_player] initializes a player with a certain index *)
let make_player id =
  (* TODO: get player name *)
  let player = Player.create ("Player " ^ string_of_int id) id in
  player

let init_players () : Player.player list =
  let p1 = make_player 1 in
  let p2 = make_player 2 in
  let p3 = make_player 3 in
  let p4 = make_player 4 in
  [ p1; p2; p3; p4 ]

(** set up starting state of mahjong: all 4 players have tiles, no discards *)
let init_tiles () =
  (* Initialize the players' hands with tiles from the shuffled deck *)
  Random.self_init ();
  let _ = Tile.init_tiles () in
  Tile.shuffle !Tile.tiles_arr

(** Initialization of main window *)
let setup () : game_board =
  Raylib.init_window window_width window_height "OCaMahJong";
  set_config_flags [ ConfigFlags.Window_resizable ];
  (* allows for user to resize screen *)
  set_target_fps 60;
  init_tiles ();
  let p_lst = init_players () in
  {
    player_lst = p_lst;
    cur_player = 0;
    (* starts /w 1st player*)
    player_hid = Player.get_hidden (List.nth p_lst 0);
    player_exp = Player.get_exposed (List.nth p_lst 0);
    discard = "";
    is_drawn = false;
    is_chi = false;
    is_peng = false;
    chi_fail = false;
    peng_fail = false;
  }

let update_game_board (gb : game_board) : unit =
  gb.player_hid <- Player.get_hidden (List.nth gb.player_lst gb.cur_player);
  gb.player_exp <- Player.get_exposed (List.nth gb.player_lst gb.cur_player);
  gb.discard <- Tile.tile_to_string (List.hd !Tile.discarded)

(** set background to green mahjong pelt

    Note: Texture2D are used to load sprites (e.g. images)*)
let draw_bg () : unit =
  let background = load_texture "res/images/mahjong_pelt.jpg" in

  (* gets max scale to ensure that bg covers entirety of window *)
  let scale_x =
    float_of_int window_width /. float_of_int (Texture2D.width background)
  in
  let scale_y =
    float_of_int window_height /. float_of_int (Texture2D.height background)
  in
  let max_scale = max scale_x scale_y in
  draw_texture_ex background (Vector2.create 0. 0.) 0. max_scale Color.white

let draw_discard dis : unit =
  let font_size = 30 in
  (* pos_x and pos_y parameters of [draw_text] is the TOP LEFT corner of
     whatever you are drawing, so must translate position based on [dis]'s
     dimensions

     no preset way of measuring height of string *)
  let dis_x = center_x - (measure_text dis font_size / 2) in
  draw_text dis dis_x center_y font_size Color.red

let draw_player_name p : unit =
  let font_size = 40 in
  let name = Player.get_name p in
  let name_x = center_x - (measure_text name font_size / 2) in
  draw_text name name_x 100 font_size Color.white

let draw_player_hid p : unit =
  let hid = Hidden_hand.hidden_hand_to_string (Player.get_hidden p) in
  let font_size = 15 in
  (* center of bottom of screen *)
  let hid_x = center_x - (measure_text hid font_size / 2) in
  (* 100 is hardcoded rn, will later change based on height of tile sprite *)
  draw_text hid hid_x (window_height - 100) font_size Color.white

let draw_player_exp p : unit =
  let exp = Exposed_hand.exposed_hand_to_string (Player.get_exposed p) in
  let font_size = 15 in
  (* above hidden hand *)
  let exp_x = center_x - (measure_text exp font_size / 2) in
  (* 150 is hardcoded rn, will later change based on height of tile sprite *)
  draw_text exp exp_x (window_height - 150) font_size Color.white

let draw_all (gb : game_board) : unit =
  (* must call to start drawing on window *)
  begin_drawing ();

  (* runs every game loop to ensure that no "traces" of objects are made *)
  clear_background Color.white;

  draw_bg ();
  draw_discard gb.discard;

  let p = List.nth gb.player_lst gb.cur_player in
  draw_player_name p;
  draw_player_hid p;
  draw_player_exp p;

  (* only render draw button when player has not drawn in turn yet && chi & peng
     have not been selected *)
  if not gb.is_drawn then draw_draw_button p gb;

  (* todo: need extra game logic == stretch - only render chi & peng buttons
     when discarded tile allows for possible combo with player hand && draw &
     the other action have not been selected*)
  if not gb.is_chi then draw_chi_button p gb;
  if not gb.is_peng then draw_peng_button p gb;
  if gb.chi_fail then draw_chi_fail ();
  if gb.peng_fail then draw_peng_fail ();

  (* only render throw button when player has already drawn/chi/peng.

     updates current player index & resets all boolean states

     tbh might not even need throw button *)
  if gb.is_drawn || gb.is_peng || gb.is_chi then
    if draw_throw_button p then (
      gb.cur_player <- (gb.cur_player + 1) mod 4;
      gb.is_drawn <- false;
      gb.is_peng <- false;
      gb.is_chi <- false);

  (* todo: toggle to next player when throw is completed *)

  (* done drawing: now show it *)
  end_drawing ()

(** Game loop: runs perpetually if user does not close window *)
let rec loop gb =
  match window_should_close () with
  | true -> close_window ()
  | false ->
      update_game_board gb;
      draw_all gb;
      loop gb

(* called when gui.exe is run from terminal *)
let () = setup () |> loop

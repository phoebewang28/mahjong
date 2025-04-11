(* GUI version of main *)

open Raylib
open Raygui
open Mahjong
open Lwt

(* todo: tiles might wanna be buttons, since we have to click to select it *)

type start_board = {
  mutable on_start_screen : bool;
  mutable player_name_inputs : string array;
  mutable player_name_edit_mode : bool array;
}

type game_board = {
  mutable player_lst : Player.player list;
  mutable cur_player_id : int; (* index of current player: 0,1,2,3 *)
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
let center_x = window_width / 2
let center_y = window_height / 2

let create_players name_arr : Player.player list =
  List.mapi (fun i name -> Player.create name (i + 1)) (Array.to_list name_arr)

let draw_chi_fail () : unit =
  draw_text "Chi was unsuccessful!" 200 200 30 Color.white

let draw_peng_fail () : unit =
  draw_text "Peng was unsuccessful!" 200 200 30 Color.white

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
    else draw_chi_fail ()
(* gb.chi_fail <- true; *)
(* i dont know how to do promise oof
      
      Lwt.async (fun () ->
          Lwt_unix.sleep 0.5 >>= fun () ->
          gb.chi_fail <- false;
          Lwt.return_unit)) *)

let draw_peng_button p gb =
  let rect =
    Raylib.Rectangle.create
      (float_of_int window_width -. 300.)
      (float_of_int window_height -. 300.)
      100.0 70.0
  in
  let is_peng_clicked = Raygui.button rect "Peng Tile" in
  if is_peng_clicked then
    if Player_choice.chi p then (
      gb.is_peng <- true;
      gb.is_chi <- true;
      gb.is_drawn <- true)
    else draw_peng_fail ()

let draw_draw_button p gb =
  let rect =
    Raylib.Rectangle.create
      (float_of_int window_width -. 300.)
      (float_of_int window_height -. 200.)
      100.0 70.0
  in
  let is_draw_clicked = Raygui.button rect "Draw Tile" in
  if is_draw_clicked then (* update player's hidden hand *)
    (
    Player_choice.draw p;
    gb.is_peng <- true;
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
  let is_throw_clicked = Raygui.button rect "Throw Tile" in
  if is_throw_clicked then (
    Player_choice.throw p;
    true)
  else false

(** [make_player id name] creates a player with the given [id] and [name]. *)
let make_player id name = Player.create name id

(** Initialize tiles *)
let init_tiles () =
  Random.self_init ();
  let _ = Tile.init_tiles () in
  Tile.shuffle !Tile.tiles_arr

(** Setup for the starting interface and game window, returning initialized
    [start_board]

    Note: should only be called ONCE -> an entire mahjong game should have a
    single [start_board] *)
let setup_start () : start_board =
  (* only called once: start board & game board uses same window *)
  set_config_flags [ ConfigFlags.Window_resizable ];
  (* todo: fix resizing bug *)
  init_window window_width window_height "OCaMahJong";
  set_target_fps 60;
  {
    player_name_inputs = [| "Player 1"; "Player 2"; "Player 3"; "Player 4" |];
    player_name_edit_mode = [| false; false; false; false |];
    on_start_screen = true;
  }

(** Setup for game interface, returning initialized [game_board]

    Note: should only be called once

    Precondition: [name_arr] must already be initialized *)
let setup_game name_arr : game_board =
  assert (Array.length name_arr <> 0);
  (* tiles must be initialized b4 players created!! *)
  init_tiles ();

  let p_lst = create_players name_arr in
  {
    player_lst = p_lst;
    cur_player_id = 0;
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

(** Updates current [gb] with new fields after player's actions *)
let update_game_board (gb : game_board) : unit =
  gb.player_hid <- Player.get_hidden (List.nth gb.player_lst gb.cur_player_id);
  gb.player_exp <- Player.get_exposed (List.nth gb.player_lst gb.cur_player_id);
  gb.discard <- Tile.tile_to_string (List.hd !Tile.discarded)

(** [draw_bg f] draws background onto window screen using image in [filename] *)
let draw_bg filename =
  let background = load_texture filename in
  let scale_x =
    float_of_int window_width /. float_of_int (Texture2D.width background)
  in
  let scale_y =
    float_of_int window_height /. float_of_int (Texture2D.height background)
  in
  (* ensures that background fits full screen *)
  let max_scale = max scale_x scale_y in
  draw_texture_ex background (Vector2.create 0. 0.) 0. max_scale Color.white

let draw_discard dis =
  let font_size = 30 in
  let dis_x = center_x - (measure_text dis font_size / 2) in
  draw_text dis dis_x center_y font_size Color.red

let draw_player_hid p : unit =
  let hid = Hidden_hand.hidden_hand_to_string (Player.get_hidden p) in
  let font_size = 15 in
  let hid_x = center_x - (measure_text hid font_size / 2) in
  draw_text hid hid_x (window_height - 100) font_size Color.white

let draw_player_exp p : unit =
  let exp = Exposed_hand.exposed_hand_to_string (Player.get_exposed p) in
  let font_size = 15 in
  let exp_x = center_x - (measure_text exp font_size / 2) in
  draw_text exp exp_x (window_height - 150) font_size Color.white

let draw_player_name p : unit =
  let font_size = 40 in
  let name = Player.get_name p in
  let name_x = center_x - (measure_text name font_size / 2) in
  draw_text name name_x 100 font_size Color.white

(** Draws all components involved in starting interface *)
let draw_all_start (sb : start_board) : unit =
  (* must call to start drawing on window *)
  begin_drawing ();
  clear_background Color.white;

  draw_bg "res/images/among-us.png";

  (* Player name input boxes *)
  let box_width = 200.0 in
  let box_height = 35.0 in
  let spacing = 15.0 in
  let total_height = (box_height +. spacing) *. 4.0 in
  let start_y = (float_of_int window_height /. 2.0) -. (total_height /. 2.0) in
  for i = 0 to 3 do
    let rect =
      Rectangle.create
        ((float_of_int window_width /. 2.0) -. (box_width /. 2.0))
        (start_y +. (float_of_int i *. (box_height +. spacing)))
        box_width box_height
    in
    let name, edit =
      Raygui.text_box rect sb.player_name_inputs.(i)
        sb.player_name_edit_mode.(i)
    in
    if edit && not sb.player_name_edit_mode.(i) then (
      for j = 0 to 3 do
        sb.player_name_edit_mode.(j) <- false
      done;
      sb.player_name_edit_mode.(i) <- true);
    sb.player_name_inputs.(i) <- name
  done;

  (* START button *)
  let button_width = 180.0 in
  let button_height = 50.0 in
  let button_x = (float_of_int window_width /. 2.0) -. (button_width /. 2.0) in
  let button_y = start_y +. (4.0 *. (box_height +. spacing)) +. 10.0 in
  let button_rect =
    Rectangle.create button_x button_y button_width button_height
  in
  let is_start_clicked = Raygui.button button_rect "START" in
  if is_start_clicked then begin
    sb.on_start_screen <- false;
    for i = 0 to 3 do
      sb.player_name_edit_mode.(i) <- false
    done
  end;

  (* done drawing: now show it *)
  end_drawing ()

(** Draws all components involved in game interface *)
let draw_all_game (gb : game_board) : unit =
  begin_drawing ();
  clear_background Color.white;

  update_game_board gb;
  draw_bg "res/images/mahjong_pelt.jpg";

  let p = List.nth gb.player_lst gb.cur_player_id in
  draw_discard gb.discard;
  draw_player_name p;
  draw_player_hid p;
  draw_player_exp p;

  (* only render draw button when player has not drawn in turn yet && chi & peng
     have not been selected *)
  if not gb.is_drawn then draw_draw_button p gb;

  (* todo: need extra game logic == stretch - only render chi & peng buttons
     when discarded tile allows for possible combo with player hand && draw &
     the other action have not been selected *)
  if not gb.is_chi then draw_chi_button p gb;
  if not gb.is_peng then draw_peng_button p gb;
  if gb.chi_fail then draw_chi_fail ();
  if gb.peng_fail then draw_peng_fail ();

  (* only render throw button when player has already drawn/chi/peng.

     updates current player index & resets all boolean states

     tbh might not even need throw button *)
  if gb.is_drawn || gb.is_peng || gb.is_chi then
    if draw_throw_button p then (
      gb.cur_player_id <- (gb.cur_player_id + 1) mod List.length gb.player_lst;
      gb.is_drawn <- false;
      gb.is_peng <- false;
      gb.is_chi <- false);

  end_drawing ()

(** Runs perpetually if user does not hit "start button" or close window.

    If user hits "start", returns array of player names. *)
let rec start_loop (sb : start_board) : string array =
  match window_should_close () with
  | true ->
      close_window ();
      exit 0 (* quit program *)
  | false ->
      if sb.on_start_screen then (
        draw_all_start sb;
        start_loop sb)
      else sb.player_name_inputs

(** Runs perpetually if user does not exit window *)
let rec game_loop (gb : game_board) =
  match window_should_close () with
  | true -> close_window ()
  | false ->
      draw_all_game gb;
      game_loop gb

let () = setup_start () |> start_loop |> setup_game |> game_loop

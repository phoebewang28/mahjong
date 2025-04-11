(* GUI version of main *)

open Raylib
open Raygui
open Mahjong
open Lwt

(* tiles might wanna be buttons, since we have to click to select it *)

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
  mutable player_name_inputs : string array;
  mutable player_name_edit_mode : bool array;
  mutable player_names : string array option;
  mutable init_done : bool;
  mutable on_start_screen : bool;
}

let window_width = 800
let window_height = 600
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
  (* note: button in raygui automatically accounts for user's mouse position &
     when clicked *)
  let is_peng_clicked = Raygui.button rect "Peng Tile" in
  if is_peng_clicked then
    if Player_choice.chi p then (
      gb.is_peng <- true;
      (* also disable other actions *)
      gb.is_chi <- true;
      gb.is_drawn <- true)
    else draw_peng_fail ()

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

(** [make_player id name] creates a player with the given [id] and [name]. *)
let make_player id name = Player.create name id

(** set up starting state of mahjong: all 4 players have tiles, no discards *)
let init_tiles () =
  (* Initialize the players' hands with tiles from the shuffled deck *)
  Random.self_init ();
  let _ = Tile.init_tiles () in
  Tile.shuffle !Tile.tiles_arr

(** Initialization of main window *)
let setup () : game_board =
  init_window window_width window_height "OCaMahJong";
  set_config_flags [ ConfigFlags.Window_resizable ];
  set_target_fps 60;
  init_tiles ();
  let p_lst = [ Player.create "placeholder" 0 ] in
  (* placeholder *)
  {
    (* TODO: should split game boards *)
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
    player_name_inputs = [| "Player 1"; "Player 2"; "Player 3"; "Player 4" |];
    player_name_edit_mode = [| false; false; false; false |];
    player_names = None;
    init_done = false;
    on_start_screen = true;
    (* chi_fail = false; peng_fail = false; *)
  }

let update_game_board (gb : game_board) : unit =
  gb.player_hid <- Player.get_hidden (List.nth gb.player_lst gb.cur_player_id);
  gb.player_exp <- Player.get_exposed (List.nth gb.player_lst gb.cur_player_id);
  gb.discard <- Tile.tile_to_string (List.hd !Tile.discarded)

let draw_background filename =
  let background = load_texture filename in
  let scale_x =
    float_of_int window_width /. float_of_int (Texture2D.width background)
  in
  let scale_y =
    float_of_int window_height /. float_of_int (Texture2D.height background)
  in
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

let draw_all (gb : game_board) : unit =
  (* must call to start drawing on window *)
  begin_drawing ();
  clear_background Color.white;

  let name_inputs = Array.copy gb.player_name_inputs in
  let edit_modes = Array.copy gb.player_name_edit_mode in
  let final_names = ref gb.player_names in
  let init_done = ref gb.init_done in
  let on_start_screen = ref gb.on_start_screen in

  if !on_start_screen then begin
    draw_background "res/images/among-us.png";

    (* Player name input boxes *)
    let box_width = 200.0 in
    let box_height = 35.0 in
    let spacing = 15.0 in
    let total_height = (box_height +. spacing) *. 4.0 in
    let start_y =
      (float_of_int window_height /. 2.0) -. (total_height /. 2.0)
    in
    for i = 0 to 3 do
      let rect =
        Rectangle.create
          ((float_of_int window_width /. 2.0) -. (box_width /. 2.0))
          (start_y +. (float_of_int i *. (box_height +. spacing)))
          box_width box_height
      in
      let name, edit = text_box rect name_inputs.(i) edit_modes.(i) in
      if edit && not edit_modes.(i) then (
        for j = 0 to 3 do
          edit_modes.(j) <- false
        done;
        edit_modes.(i) <- true);
      name_inputs.(i) <- name
    done;

    (* START button *)
    let button_width = 180.0 in
    let button_height = 50.0 in
    let button_x =
      (float_of_int window_width /. 2.0) -. (button_width /. 2.0)
    in
    (* TODO: can simplify by using button object in raygui *)
    let button_y = start_y +. (4.0 *. (box_height +. spacing)) +. 10.0 in
    let button_rect =
      Rectangle.create button_x button_y button_width button_height
    in
    let label = "START" in
    let label_font_size = 24 in
    let label_text_width = measure_text label label_font_size in
    let label_x =
      button_x +. (button_width /. 2.0) -. (float_of_int label_text_width /. 2.0)
    in
    let label_y =
      button_y +. (button_height /. 2.0) -. (float_of_int label_font_size /. 2.0)
    in
    draw_rectangle_rounded button_rect 0.2 10 Color.darkgray;
    draw_text label (int_of_float label_x) (int_of_float label_y)
      label_font_size Color.white;
    if
      check_collision_point_rec (get_mouse_position ()) button_rect
      && is_mouse_button_pressed MouseButton.Left
    then begin
      final_names := Some name_inputs;
      (* create players here *)
      gb.player_lst <-
        List.mapi
          (fun i name -> Player.create name (i + 1))
          (Array.to_list name_inputs);
      init_done := true;
      on_start_screen := false;
      for i = 0 to 3 do
        edit_modes.(i) <- false
      done
    end;
    Raygui.set_style (Button `Text_padding) 10;

    (* reset padding *)
    gb.player_name_inputs <- name_inputs;
    gb.player_name_edit_mode <- edit_modes;
    gb.player_names <- !final_names;
    gb.init_done <- !init_done;
    gb.on_start_screen <- !on_start_screen
  end
  else begin
    update_game_board gb;
    draw_background "res/images/mahjong_pelt.jpg";
    let p = List.nth gb.player_lst gb.cur_player_id in
    draw_discard gb.discard;
    draw_player_name p;
    draw_player_hid p;
    draw_player_exp p;

    (* only render draw button when player has not drawn in turn yet && chi &
       peng have not been selected *)
    if not gb.is_drawn then draw_draw_button p gb;

    (* todo: need extra game logic == stretch - only render chi & peng buttons
       when discarded tile allows for possible combo with player hand && draw &
       the other action have not been selected*)
    if not gb.is_chi then draw_chi_button p gb;
    if not gb.is_peng then draw_peng_button p gb;
    if gb.chi_fail then draw_chi_fail ();
    if gb.peng_fail then draw_peng_fail ();

    (* only render throw button when player has already drawn/chi/peng. updates
       current player index & resets all boolean states tbh might not even need
       throw button *)
    if gb.is_drawn || gb.is_peng || gb.is_chi then
      if draw_throw_button p then (
        gb.cur_player_id <- (gb.cur_player_id + 1) mod 4;
        (*hardcoded 4 players*)
        gb.is_drawn <- false;
        gb.is_peng <- false;
        gb.is_chi <- false)
  end;

  (* done drawing: now show it *)
  end_drawing ()

(* player_name_inputs = name_inputs; player_name_edit_mode = edit_modes;
   player_names = !final_names; init_done = !init_done; on_start_screen =
   !on_start_screen; *)

(** Game loop: runs perpetually if user does not close window *)
let rec loop gb =
  match window_should_close () with
  | true -> close_window ()
  | false ->
      draw_all gb;
      loop gb

let () = setup () |> loop

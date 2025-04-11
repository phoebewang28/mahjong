(* GUI version of main *)

open Raylib
open Raygui
open Mahjong

type game_board = {
  player_hid : string;
  player_exp : string;
  discard : string;
  player_name_inputs : string array;
  player_name_edit_mode : bool array;
  player_names : string array option;
  init_done : bool;
  current_player_index : int;
  on_start_screen : bool;
  players : Player.player array option;
}
(** [game_board] represents the state of the visible game interface. *)

let window_width = 800
let window_height = 600
let center_x = window_width / 2
let center_y = window_height / 2

(** [make_player id name] creates a player with the given [id] and [name]. *)
let make_player id name = Player.create name id

(** Initializes player hands and the tile pool. *)
let init_tiles () =
  Random.self_init ();
  let _ = Tile.init_tiles () in
  Tile.shuffle !Tile.tiles_arr;
  let p1 = make_player 1 "Placeholder" in
  ( Hidden_hand.hidden_hand_to_string (Player.get_hidden p1),
    Exposed_hand.exposed_hand_to_string (Player.get_exposed p1) )

(** Initializes the game window and GUI state *)
let setup () : game_board =
  init_window window_width window_height "OCaMahJong";
  set_config_flags [ ConfigFlags.Window_resizable ];
  set_target_fps 60;
  let p_hid, p_exp = init_tiles () in
  {
    player_hid = p_hid;
    player_exp = p_exp;
    discard = "discarded";
    player_name_inputs = [| "Player 1"; "Player 2"; "Player 3"; "Player 4" |];
    player_name_edit_mode = [| false; false; false; false |];
    player_names = None;
    init_done = false;
    current_player_index = 0;
    on_start_screen = true;
    players = None;
  }

(** Updates the game board state. *)
let update_game_board (gb : game_board) : game_board = gb

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

let draw_player_hid hid =
  let font_size = 15 in
  let hid_x = center_x - (measure_text hid font_size / 2) in
  draw_text hid hid_x (window_height - 100) font_size Color.white

let draw_player_exp exp =
  let font_size = 15 in
  let exp_x = center_x - (measure_text exp font_size / 2) in
  draw_text exp exp_x (window_height - 150) font_size Color.white

let draw_player_name (names_opt : string array option) (idx : int) =
  match names_opt with
  | Some names ->
      let name = names.(idx) in
      let font_size = 20 in
      let name_x = center_x - (measure_text name font_size / 2) in
      draw_text name name_x (center_y - 50) font_size Color.black
  | None -> ()

(** Main GUI drawing logic *)
let draw_all (gb : game_board) : game_board =
  let players = ref gb.players in
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
      let created_players =
        Array.mapi (fun i name -> Player.create name (i + 1)) name_inputs
      in
      init_done := true;
      on_start_screen := false;
      players := Some created_players;
      for i = 0 to 3 do
        edit_modes.(i) <- false
      done
    end;
    Raygui.set_style (Button `Text_padding) 10
    (* reset padding *)
  end
  else begin
    draw_background "res/images/mahjong_pelt.jpg";
    draw_discard gb.discard;
    draw_player_hid gb.player_hid;
    draw_player_exp gb.player_exp;
    draw_player_name gb.player_names gb.current_player_index
  end;

  end_drawing ();
  {
    gb with
    player_name_inputs = name_inputs;
    player_name_edit_mode = edit_modes;
    player_names = !final_names;
    init_done = !init_done;
    on_start_screen = !on_start_screen;
    players = !players;
  }

let rec loop (gb : game_board) =
  match window_should_close () with
  | true -> close_window ()
  | false -> update_game_board gb |> draw_all |> loop

let () = setup () |> loop

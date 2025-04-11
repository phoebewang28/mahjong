(* GUI version of main *)

open Raylib
open Raygui
open Mahjong

type game_board = {
  player_hid : string; (* hidden hand of CURRENT player *)
  player_exp : string;
  discard : string; (*for now, just one*)
}

let window_width = 800
let window_height = 600

(** Center of the window *)
let center_x = window_width / 2

let center_y = window_height / 2
(* 
let draw_draw_button : unit =
  let rect = Raylib.Rectangle.create (float_of_int window_width -. 100.) (float_of_int window_height -. 200.) 40.0 20.0 in
  Raygui.button rect "Draw Tile" *)


(** [make_player] initializes a player with a certain index *)
let make_player id =
  (* TODO: get player name *)
  let player = Player.create "Player placeholder name" id in
  player


(** set up starting state of mahjong: all 4 players have tiles, no discards 

returns value for player_hid & player_exp fields of game_board in a tuple *)
let init_tiles () = 
    (* Initialize the players' hands with tiles from the shuffled deck *)
    Random.self_init ();
    let _ = Tile.init_tiles () in
    Tile.shuffle !Tile.tiles_arr;
    let p1 = make_player 1 in
    (* let p2 = make_player 2 in
    let p3 = make_player 3 in
    let p4 = make_player 4 in *)
    ((Hidden_hand.hidden_hand_to_string (Player.get_hidden p1)), (Exposed_hand.exposed_hand_to_string (Player.get_exposed p1)))



(** type definition containing every object that is within game environment
    (aka. on screen)

    no need to draw player themselves cuz they're just mouse location

    To be added:
    - other players
    - other discarded tiles (not just most recent)
    - buttons : string list (change to use Raylib's button instead) *)

(** Initialization of main window *)
let setup () : game_board =
  Raylib.init_window window_width window_height "OCaMahJong";
  set_config_flags [ ConfigFlags.Window_resizable ];
  (* allows for user to resize screen *)
  set_target_fps 60;
  let (p_hid, p_exp) = init_tiles () in
  {
    player_hid = p_hid;
    player_exp = p_exp;
    discard = "discarded";
  }

(** should be upated every time a player moves; if player hasn't chosen move, no
    need to update yet *)
let update_game_board (gb : game_board) : game_board =
  {
    player_hid = gb.player_hid; (* same rn, will update later once player can move in gui *)
    player_exp = gb.player_exp;
    discard = gb.discard;
  }

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

let draw_player_hid hid : unit =
  let font_size = 15 in
  (* center of bottom of screen *)
  let hid_x = center_x - (measure_text hid font_size / 2) in
  (* 100 is hardcoded rn, will later change based on height of tile sprite *)
  draw_text hid hid_x (window_height - 100) font_size Color.white

let draw_player_exp exp : unit =
  let font_size = 15 in
  (* above hidden hand *)
  let exp_x = center_x - (measure_text exp font_size / 2) in
  (* 150 is hardcoded rn, will later change based on height of tile sprite *)
  draw_text exp exp_x (window_height - 150) font_size Color.white

(** returns game_board so that next loop can update again *)
let draw_all (gb : game_board) : game_board =
  (* must call to start drawing on window *)
  begin_drawing ();

  (* runs every game loop to ensure that no "traces" of objects are made *)
  clear_background Color.white;

  draw_bg ();
  draw_discard gb.discard;
  draw_player_hid gb.player_hid;
  draw_player_exp gb.player_exp;

  (* done drawing: now show it *)
  end_drawing ();

  {
    player_hid = gb.player_hid;
    player_exp = gb.player_exp;
    discard = gb.discard;
  }

(** Game loop: runs perpetually if user does not close window *)
let rec loop (gb : game_board) =
  match window_should_close () with
  | true -> close_window ()
  | false -> update_game_board gb |> draw_all |> loop

(* called when gui.exe is run from terminal *)
let () = setup () |> loop

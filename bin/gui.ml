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

(* Tile image handling *)
let tile_image_table : (string, Texture2D.t) Hashtbl.t = Hashtbl.create 50

let tile_keys =
  [
    "1tong";
    "2tong";
    "3tong";
    "4tong";
    "5tong";
    "6tong";
    "7tong";
    "8tong";
    "9tong";
    "1wan";
    "2wan";
    "3wan";
    "4wan";
    "5wan";
    "6wan";
    "7wan";
    "8wan";
    "9wan";
    "1tiao";
    "2tiao";
    "3tiao";
    "4tiao";
    "5tiao";
    "6tiao";
    "7tiao";
    "8tiao";
    "9tiao";
    "dong";
    "nan";
    "xi";
    "bei";
    "zhong";
    "fa";
    "bai";
  ]

let load_tile_images () =
  List.iter
    (fun key ->
      let path = Printf.sprintf "res/images/tile/%s.png" key in
      let tex = load_texture path in
      Hashtbl.add tile_image_table key tex;
      print_endline (Printf.sprintf "Loaded %s" key))
    tile_keys

let get_tile_texture key = Hashtbl.find_opt tile_image_table key

let draw_tile_list_from_keys keys x0 y0 =
  let scale = 0.04 in
  let x = ref (float_of_int x0) in
  List.iter
    (fun key ->
      match get_tile_texture key with
      | Some tex ->
          let tex_w = float_of_int (Texture2D.width tex) *. scale in
          draw_texture_ex tex
            (Vector2.create !x (float_of_int y0))
            0.0 scale Color.white;
          x := !x +. tex_w
      | None ->
          draw_text " " (int_of_float !x) y0 20 Color.red;
          x := !x +. 40.0)
    keys

type game_board = {
  mutable player_lst : Player.player list;
  mutable cur_player_id : int; (* index of current player: 0,1,2,3 *)
  mutable player_hid : Hidden_hand.hidden_hand;
      (* hidden hand of CURRENT player *)
  mutable player_exp : Exposed_hand.exposed_hand;
  mutable discard : Tile.tile option;
  mutable is_drawn : bool;
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
    if Player_choice.chi p then gb.is_chi <- true else draw_chi_fail ()
(* gb.chi_fail <- true; *)
(* i dont know how to do promise oof

   Lwt.async (fun () -> Lwt_unix.sleep 0.5 >>= fun () -> gb.chi_fail <- false;
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
    if Player_choice.peng p then gb.is_peng <- true else draw_peng_fail ()

let draw_draw_button p gb =
  let rect =
    Raylib.Rectangle.create
      (float_of_int window_width -. 300.)
      (float_of_int window_height -. 200.)
      100.0 70.0
  in
  let is_draw_clicked = Raygui.button rect "Draw Tile" in
  if is_draw_clicked then (
    (* update player's hidden hand *)
    Player_choice.draw p;
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
  load_tile_images ();
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
  let first_player = List.nth p_lst 0 in
  {
    player_lst = p_lst;
    cur_player_id = 0;
    (* starts /w 1st player*)
    player_hid = Player.get_hidden first_player;
    player_exp = Player.get_exposed first_player;
    discard = None;
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
  gb.discard <-
    (if
       gb.is_peng || gb.is_chi
       (* because most recently discarded is taken out of center, prevents it
          from rendering *)
     then None
     else if List.length !Tile.discarded > 0 then
       let dis = List.hd !Tile.discarded in
       if Tile.tile_to_string dis = "3110 Fake" then None
       else Some (List.hd !Tile.discarded)
     else None)

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

(* let draw_discard dis = let font_size = 30 in let dis_x = center_x -
   (measure_text dis font_size / 2) in draw_text dis dis_x center_y font_size
   Color.red *)

let draw_discard_tile (tile_opt : Tile.tile option) =
  match tile_opt with
  | Some tile -> (
      let scale = 0.04 in
      let key = Tile.tile_to_key tile in
      let x = float_of_int center_x in
      let y = float_of_int center_y in
      match get_tile_texture key with
      | Some tex ->
          draw_texture_ex tex (Vector2.create x y) 0.0 scale Color.white
      | None -> draw_text " " (int_of_float x) (int_of_float y) 30 Color.red)
  | None -> ()

let draw_player_hid p : unit =
  let hidden_hand = Player.get_hidden p in
  let tiles = Hidden_hand.get_tiles hidden_hand in
  let keys = Tile.tile_list_to_keys tiles in
  draw_tile_list_from_keys keys (center_x - 350) (window_height - 100)

let draw_player_exp p : unit =
  let exposed_hand = Player.get_exposed p in
  let tiles = Exposed_hand.get_tiles exposed_hand in
  let keys = Tile.tile_list_to_keys tiles in
  Printf.printf "Exposed hand keys:\n";
  List.iter (fun k -> Printf.printf " - %s\n" k) keys;
  draw_tile_list_from_keys keys (center_x - 300) (window_height - 200)

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
  draw_discard_tile gb.discard;
  draw_player_name p;
  draw_player_hid p;
  draw_player_exp p;

  (* only render draw/chi/peng button when player has not selected ANY of
     drawn/chi/peng actions. If any one selected, all buttons disabled -> only
     THROW enabled.*)
  if (not gb.is_drawn) && (not gb.is_chi) && not gb.is_peng then (
    draw_draw_button p gb;
    draw_chi_button p gb;
    draw_peng_button p gb)
  else if
    (* draws throw button & swaps to next player if throw successful *)
    draw_throw_button p
  then (
    gb.cur_player_id <- (gb.cur_player_id + 1) mod List.length gb.player_lst;
    gb.is_drawn <- false;
    gb.is_peng <- false;
    gb.is_chi <- false);

  (* todo: need extra game logic == stretch - only render chi & peng buttons
     when discarded tile allows for possible combo with player hand && draw &
     the other action have not been selected *)
  if gb.chi_fail then draw_chi_fail ();
  if gb.peng_fail then draw_peng_fail ();

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

open Tile
open Hidden_hand
open Exposed_hand

type player = {
  name : string;
  index : int;
  mutable money : int;
  mutable hidden : hidden_hand;
  mutable exposed : exposed_hand;
}

let make_player n i m hh eh =
  { name = n; index = i; money = m; hidden = hh; exposed = eh }

let create name index =
  let player =
    {
      name;
      index;
      money = 1500;
      hidden =
        init_hidden_hand
          (Array.to_list (Array.sub !Tile.tiles_arr !Tile.curr_index 13));
      exposed = empty_exposed_hand ();
    }
  in
  Tile.curr_index := !Tile.curr_index + 13;
  player

let make_player n i m hh eh =
  { name = n; index = i; money = m; hidden = hh; exposed = eh }

let get_index p = p.index
let get_money p = p.money
let get_name p = p.name
let get_hidden p = p.hidden
let get_exposed p = p.exposed
let set_money amt p = p.money <- amt

let winner p player_list bet =
  set_money (get_money p + (4 * bet)) p;
  List.iter (fun a -> set_money (get_money a - bet) a) player_list

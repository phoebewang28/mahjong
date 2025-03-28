open Tile

type player = {
  name : string;
  index : int;
  mutable money : int;
  mutable hidden : tile array;
  mutable exposed : (group * int) list;
}

let create name index =
  {
    name;
    index;
    money = 1500;
    hidden = [||];
    exposed = [];
  }

let deal player =
  player.hidden <- Array.sub Tile.tiles_arr !(Tile.curr_index) 13;
  Tile.curr_index := !(Tile.curr_index) + 13
let get_index p = p.index
let get_money p = p.money

let get_name p = p.name
let get_hidden p = p.hidden

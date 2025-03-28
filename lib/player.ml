open Tile

type player = {
  name : string;
  index : int;
  mutable money : int;
  mutable hidden : tile option array;
  mutable exposed : (group * int) list;
}

let create name index =
  {
    name;
    index;
    money = 1500;
    hidden = Array.make 14 (make_tile { num = 3110; tao = Tong });
    exposed = [];
  }

let get_index p = p.index
let get_money p = p.money

let get_hidden p = p.hidden

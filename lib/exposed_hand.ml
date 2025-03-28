open Tile

(* A.F.: list of p groups [(g1, n1);...; (gp, np)] represents legal sets in
   player’s exposed hand R.I.: p (size of list) ≤ 4 *)
type exposed_hand = (group * tile) list

let chi (t : tile) (e : exposed_hand) : exposed_hand = (make_group "Shun", t) :: e

let peng (t : tile) (e : exposed_hand) : exposed_hand = (make_group "San", t) :: e
let ming_gang (t : tile) (e : exposed_hand) : exposed_hand = (make_group "Si", t) :: e


let exposed_hand_to_string (e : exposed_hand) : string =
  String.concat ", "
    (List.map
       (fun (g, t) ->
         Printf.sprintf "%s %s" (tile_to_string t) (group_to_string g))
       e)

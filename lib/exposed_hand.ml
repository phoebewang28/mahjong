open Tile

type exposed_hand = (group * tile) list ref
(** - A.F.: list of p groups [(g1, n1);...; (gp, np)] represents legal sets in
      player’s exposed hand
    - R.I.: p (size of list) ≤ 4 *)

let chi (t : tile) (e : exposed_hand) : unit = e := (make_group "Shun", t) :: !e
let peng (t : tile) (e : exposed_hand) : unit = e := (make_group "San", t) :: !e

let ming_gang (t : tile) (e : exposed_hand) : unit =
  e := (make_group "Si", t) :: !e

let exposed_hand_to_string (e : exposed_hand) : string =
  String.concat ", "
    (List.map
       (fun (g, t) ->
         Printf.sprintf "%s %s" (tile_to_string t) (group_to_string g))
       !e)

let get_tiles (e : exposed_hand) : tile list =
  List.flatten
    (List.map
       (fun (g, t) ->
         match group_to_string g with
         | "ShunZi" ->
             let n = get_num t in
             if n <= 7 then
               [
                 make_tile n (get_tao t);
                 make_tile (n + 1) (get_tao t);
                 make_tile (n + 2) (get_tao t);
               ]
             else []
         | "KeZi" -> [ t; t; t ]
         | "Gang" -> [ t; t; t; t ]
         | _ -> [])
       !e)

(** [empty_exposed_hand] is an empty exposed hand. *)
let empty_exposed_hand () : exposed_hand = ref []

let exposed_hand_count (e : exposed_hand) : int = List.length !e

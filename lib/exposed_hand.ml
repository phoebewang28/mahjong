type group =
  | Shun
  | San
  | Si

type da_pai =
  | Dong
  | Nan
  | Xi
  | Bei
  | Zhong
  | Fa
  | Bai

type suit =
  | Tong
  | Wan
  | Tiao
  | DaPai of da_pai

type tile = {
  num : int;
  tao : suit;
}

type exposed_hand = (group * tile) list

let chi_left (t : tile) (e : exposed_hand) : exposed_hand = (Shun, t) :: e

let chi_middle (t : tile) (e : exposed_hand) : exposed_hand =
  (Shun, { num = t.num - 1; tao = t.tao }) :: e

let chi_right (t : tile) (e : exposed_hand) : exposed_hand =
  (Shun, { num = t.num - 2; tao = t.tao }) :: e

let peng (t : tile) (e : exposed_hand) : exposed_hand = (San, t) :: e
let ming_gang (t : tile) (e : exposed_hand) : exposed_hand = (Si, t) :: e

let tile_to_string (t : tile) : string =
  Printf.sprintf "%s%s"
    (if t.num = 0 then "" else string_of_int t.num ^ " ")
    (match t.tao with
    | Tong -> "Tong"
    | Wan -> "Wan"
    | Tiao -> "Tiao"
    | DaPai dp -> (
        match dp with
        | Dong -> "Dong"
        | Nan -> "Nan"
        | Xi -> "Xi"
        | Bei -> "Bei"
        | Zhong -> "Zhong"
        | Fa -> "Fa"
        | Bai -> "Bai"))

let group_to_string (g : group) : string =
  match g with
  | Shun -> "ShunZi"
  | San -> "KeZi"
  | Si -> "Gang"

let exposed_hand_to_string (e : exposed_hand) : string =
  String.concat ", "
    (List.map
       (fun (g, t) ->
         Printf.sprintf "%s %s" (tile_to_string t) (group_to_string g))
       e)

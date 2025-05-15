exception PlayerWin of Player.player

let filter_hidden player =
  List.filter
    (fun x -> Tile.suit_to_string (Tile.get_tao x) <> "Fake")
    (Hidden_hand.get_tiles (Player.get_hidden player))

(** [count_same tiles tile] counts the number of occurrences of [tile] in the
    list [tiles]. *)
let count_same tiles tile =
  List.length (List.filter (fun t -> Tile.compare_tile t tile = 0) tiles)

(** [remove_n tiles t n] removes [n] occurrences of [t] from the list [tiles].
*)
let rec remove_n tiles t n : Tile.tile list =
  match (tiles, n) with
  | [], _ -> []
  | _, 0 -> tiles
  | h :: tl, _ -> if h = t then remove_n tl t (n - 1) else h :: remove_n tl t n

(** [is_shu t] checks if the tile [t] belongs to the suits "Wan", "Tong", or
    "Tiao". *)
let is_shu t =
  match Tile.suit_to_string (Tile.get_tao t) with
  | "Wan" | "Tong" | "Tiao" -> true
  | _ -> false

(** [remove_tiles tiles ts] removes all tiles in [ts] from the list [tiles]. *)
let rec remove_tiles tiles ts =
  match ts with
  | [] -> tiles
  | r :: rs -> remove_tiles (remove_n tiles r 1) rs

(** [try_make_melds tiles n] checks if [tiles] can form [n] melds (triples or
    sequences). *)
let rec try_make_melds tiles n =
  if n = 0 then tiles = []
  else
    match tiles with
    | [] -> false
    | t :: _ ->
        let attempt_triple =
          if count_same tiles t >= 3 then
            let rest = remove_n tiles t 3 in
            try_make_melds rest (n - 1)
          else false
        in
        let attempt_sequence =
          if is_shu t && Tile.get_num t <= 7 then
            let t2 = Tile.make_tile (Tile.get_num t + 1) (Tile.get_tao t) in
            let t3 = Tile.make_tile (Tile.get_num t + 2) (Tile.get_tao t) in
            if List.mem t2 tiles && List.mem t3 tiles then
              let rest = remove_tiles tiles [ t; t2; t3 ] in
              try_make_melds rest (n - 1)
            else false
          else false
        in
        attempt_triple || attempt_sequence

(** [is_wind t] checks if the tile [t] is a wind tile ("Dong", "Nan", "Xi",
    "Bei"). *)
let is_wind t =
  match Tile.suit_to_string (Tile.get_tao t) with
  | "Dong" | "Nan" | "Xi" | "Bei" -> true
  | _ -> false

(** [is_wind_group (g, t)] checks if the group [g] with tile [t] is a wind
    group. *)
let is_wind_group (g, t) =
  match Tile.group_to_string g with
  | "Shun" -> false
  | "San" | "Si" -> is_wind t
  | _ -> false

let complete (p : Player.player) : bool =
  let hidden = filter_hidden p in
  let exposed_blocks = Exposed_hand.exposed_hand_count (Player.get_exposed p) in
  let needed = 4 - exposed_blocks in

  (* Try each possible pair, and see if the remaining tiles can form the needed
     melds *)
  let rec try_all_pairs = function
    | [] -> false
    | t :: rest ->
        if count_same hidden t >= 2 then
          let remaining = remove_n hidden t 2 in
          if try_make_melds remaining needed then true
          else
            try_all_pairs
              (List.filter (fun x -> Tile.compare_tile x t <> 0) rest)
        else try_all_pairs rest
  in

  (* Get unique tiles to try as pairs *)
  let unique_tiles = List.sort_uniq Tile.compare_tile hidden in
  try_all_pairs unique_tiles

(** [has_pon_or_kan p] checks if the player [p] has any exposed "Pon" or "Kan"
    groups. *)
let has_pon_or_kan (p : Player.player) : bool =
  List.exists
    (fun g ->
      match Tile.group_to_string g with
      | "San" | "Si" -> true
      | _ -> false)
    (Exposed_hand.get_groups (Player.get_exposed p))

(** [try_make_sequence_only tiles n] checks if [tiles] can form [n] sequences
    only. *)
let rec try_make_sequence_only tiles n =
  if n = 0 then tiles = []
  else
    match tiles with
    | [] -> false
    | t :: _ ->
        if is_shu t && Tile.get_num t <= 7 then
          let t2 = Tile.make_tile (Tile.get_num t + 1) (Tile.get_tao t) in
          let t3 = Tile.make_tile (Tile.get_num t + 2) (Tile.get_tao t) in
          if List.mem t2 tiles && List.mem t3 tiles then
            let rest = remove_tiles tiles [ t; t2; t3 ] in
            try_make_sequence_only rest (n - 1)
          else false
        else false

let pinghu (p : Player.player) : bool =
  if complete p then
    if has_pon_or_kan p then false
    else
      let hidden = filter_hidden p in
      let exposed_blocks =
        Exposed_hand.exposed_hand_count (Player.get_exposed p)
      in
      let needed = 4 - exposed_blocks in

      (* Check if the hidden hand can form the needed melds *)
      let rec try_all_pairs = function
        | [] -> false
        | t :: rest ->
            if count_same hidden t >= 2 then
              let remaining =
                List.sort Tile.compare_tile (remove_n hidden t 2)
              in
              if try_make_sequence_only remaining needed then true
              else
                try_all_pairs
                  (List.filter (fun x -> Tile.compare_tile x t <> 0) rest)
            else try_all_pairs rest
      in
      let unique_tiles = List.sort_uniq Tile.compare_tile hidden in
      try_all_pairs unique_tiles
  else false

(** [try_make_wind_only tiles n] checks if [tiles] can form [n] wind melds only.
*)
let rec try_make_wind_only tiles n =
  if n = 0 then true
  else
    match tiles with
    | [] -> false
    | t :: _ ->
        if is_wind t then
          if count_same tiles t >= 3 then
            let rest = remove_n tiles t 3 in
            try_make_wind_only rest (n - 1)
          else false
        else false

let dasixi (p : Player.player) : bool =
  if complete p then
    let hidden = filter_hidden p in
    let exposed = Exposed_hand.get_exposed_hand (Player.get_exposed p) in
    let exposed_count = List.length (List.filter is_wind_group exposed) in
    let needed = 4 - exposed_count in
    let rec try_all_pairs = function
      | [] -> false
      | t :: rest ->
          if count_same hidden t >= 2 then
            let remaining = List.sort Tile.compare_tile (remove_n hidden t 2) in
            if try_make_wind_only remaining needed then true
            else
              try_all_pairs
                (List.filter (fun x -> Tile.compare_tile x t <> 0) rest)
          else try_all_pairs rest
    in
    let unique_tiles = List.sort_uniq Tile.compare_tile hidden in
    try_all_pairs unique_tiles
  else false

(** [is_dragon t] checks if the tile [t] is a dragon tile ("Zhong", "Fa",
    "Bai"). *)
let is_dragon t =
  match Tile.suit_to_string (Tile.get_tao t) with
  | "Zhong" | "Fa" | "Bai" -> true
  | _ -> false

(** [is_dragon_group (g, t)] checks if the group [g] with tile [t] is a dragon
    group. *)
let is_dragon_group (g, t) =
  match Tile.group_to_string g with
  | "Shun" -> false
  | "San" | "Si" -> is_dragon t
  | _ -> false

(** [try_make_dragon_only tiles n] checks if [tiles] can form [n] dragon melds
    only. *)
let rec try_make_dragon_only tiles n =
  if n = 0 then true
  else
    match tiles with
    | [] -> false
    | x :: xs ->
        if is_dragon x then
          if count_same tiles x >= 3 then
            let rest = remove_n tiles x 3 in
            try_make_dragon_only rest (n - 1)
          else false
        else try_make_dragon_only xs n

let dasanyuan (p : Player.player) : bool =
  if complete p then
    let hidden = filter_hidden p in
    let exposed = Exposed_hand.get_exposed_hand (Player.get_exposed p) in
    let exposed_count = List.length (List.filter is_dragon_group exposed) in
    let needed = 3 - exposed_count in
    let rec try_all_pairs = function
      | [] -> false
      | t :: rest ->
          if is_dragon t then try_all_pairs rest
          else if count_same hidden t >= 2 then
            let remaining = List.sort Tile.compare_tile (remove_n hidden t 2) in
            if try_make_dragon_only remaining needed then true
            else
              try_all_pairs
                (List.filter (fun x -> Tile.compare_tile x t <> 0) rest)
          else try_all_pairs rest
    in
    let unique_tiles = List.sort_uniq Tile.compare_tile hidden in
    try_all_pairs unique_tiles
  else false

(** [is_lv t] checks if the tile [t] is a green tile ("Fa" or "Tiao" tiles with
    num 2, 3, 4, 6, 8 ). *)
let is_lv t =
  match Tile.suit_to_string (Tile.get_tao t) with
  | "Fa" -> true
  | "Tiao" ->
      let num = Tile.get_num t in
      if num = 2 || num = 3 || num = 4 || num = 6 || num = 8 then true
      else false
  | _ -> false

let lvyise (p : Player.player) : bool =
  if complete p then
    let hidden = filter_hidden p in
    let exposed = Exposed_hand.get_tiles (Player.get_exposed p) in
    let hand = hidden @ exposed in
    List.length (List.filter is_lv hand) = 14
  else false

let qidui (p : Player.player) : bool =
  let hidden = filter_hidden p in
  let exposed = Exposed_hand.get_tiles (Player.get_exposed p) in
  let hand = hidden @ exposed in
  let unique_tiles = List.sort_uniq Tile.compare_tile hand in
  List.for_all
    (fun t -> count_same hand t = 2 || count_same hand t = 4)
    unique_tiles

(** [is_consecutive tiles] checks if the tiles in [tiles] form a consecutive
    sequence(from 2 to 8). *)
let is_consecutive tiles =
  let rec aux acc = function
    | [] -> acc
    | t :: [] -> acc + 1
    | t1 :: t2 :: rest ->
        if Tile.get_num t2 - Tile.get_num t1 = 1 then aux (acc + 1) (t2 :: rest)
        else acc
  in
  aux 0 tiles = 7

(** [is_tong tiles] filters the tiles in [tiles] to only include "Tong" tiles.
*)
let is_tong tiles =
  List.filter
    (fun t ->
      match Tile.suit_to_string (Tile.get_tao t) with
      | "Tong" -> true
      | _ -> false)
    tiles

(** [is_tiao tiles] filters the tiles in [tiles] to only include "Tiao" tiles.
*)
let is_tiao tiles =
  List.filter
    (fun t ->
      match Tile.suit_to_string (Tile.get_tao t) with
      | "Tiao" -> true
      | _ -> false)
    tiles

(** [is_wan tiles] filters the tiles in [tiles] to only include "Wan" tiles. *)
let is_wan tiles =
  List.filter
    (fun t ->
      match Tile.suit_to_string (Tile.get_tao t) with
      | "Wan" -> true
      | _ -> false)
    tiles

(** [is_single_kind tiles] checks if all tiles in [tiles] belong to the same
    suit. *)
let is_single_kind tiles =
  List.length (is_wan tiles) = List.length tiles
  || List.length (is_tiao tiles) = List.length tiles
  || List.length (is_tong tiles) = List.length tiles

let jiulianbaodeng (p : Player.player) : bool =
  if complete p then
    let hidden = filter_hidden p in
    let exposed = Exposed_hand.get_tiles (Player.get_exposed p) in
    let hand = hidden @ exposed in
    if is_single_kind hand then
      let tao = Tile.get_tao (List.hd hand) in
      let c1 = count_same hand (Tile.make_tile 1 tao) in
      let c9 = count_same hand (Tile.make_tile 9 tao) in
      if c1 >= 3 && c9 >= 3 then
        let hand = remove_n hand (Tile.make_tile 1 tao) c1 in
        let hand = remove_n hand (Tile.make_tile 9 tao) c9 in
        let unique_tiles = List.sort_uniq Tile.compare_tile hand in
        is_consecutive unique_tiles
      else false
    else false
  else false

(** [try_make_melds_count tiles n a] checks if [tiles] can form [n] melds with
    [a] Kezi melds. *)
let rec try_make_melds_count tiles n a =
  if n = 0 && a = 0 then true
  else
    match tiles with
    | [] -> false
    | t :: _ ->
        let attempt_triple =
          if count_same tiles t >= 3 then
            let rest = remove_n tiles t 3 in
            try_make_melds_count rest (n - 1) (a - 1)
          else false
        in
        let attempt_sequence =
          if is_shu t && Tile.get_num t <= 7 then
            let t2 = Tile.make_tile (Tile.get_num t + 1) (Tile.get_tao t) in
            let t3 = Tile.make_tile (Tile.get_num t + 2) (Tile.get_tao t) in
            if List.mem t2 tiles && List.mem t3 tiles then
              let rest = remove_tiles tiles [ t; t2; t3 ] in
              try_make_melds_count rest (n - 1) a
            else false
          else false
        in
        attempt_triple || attempt_sequence

(** [try_make_kezi tiles n a] checks if [tiles] can form [n] melds with [a]
    Kezi. *)
let try_make_kezi tiles n a =
  let rec try_all_pairs = function
    | [] -> false
    | t :: rest ->
        if count_same tiles t >= 2 then
          let remaining = remove_n tiles t 2 in
          if try_make_melds_count remaining n a then true
          else
            try_all_pairs
              (List.filter (fun x -> Tile.compare_tile x t <> 0) rest)
        else try_all_pairs rest
  in

  (* Get unique tiles to try as pairs *)
  let unique_tiles = List.sort_uniq Tile.compare_tile tiles in
  try_all_pairs unique_tiles

let is_wind t =
  match Tile.suit_to_string (Tile.get_tao t) with
  | "Dong" | "Nan" | "Xi" | "Bei" -> true
  | _ -> false

let duiduihu (p : Player.player) : bool =
  if complete p then
    let hidden = filter_hidden p in
    let exposed = Exposed_hand.get_tiles (Player.get_exposed p) in
    let hand = hidden @ exposed in
    try_make_kezi hand 4 4
  else false

let kankanhu (p : Player.player) : bool =
  if complete p then
    let hidden = filter_hidden p in
    try_make_kezi hidden 4 4
  else false

let qingyise (p : Player.player) : bool =
  if complete p || qidui p then
    let hidden = filter_hidden p in
    let exposed = Exposed_hand.get_tiles (Player.get_exposed p) in
    let hand = hidden @ exposed in
    is_single_kind hand
  else false

(** [is_zi tiles] filters the tiles in [tiles] to only include honor tiles. *)
let is_zi tiles =
  List.filter
    (fun t ->
      match Tile.suit_to_string (Tile.get_tao t) with
      | "Dong" | "Nan" | "Xi" | "Bei" | "Zhong" | "Fa" | "Bai" -> true
      | _ -> false)
    tiles

let ziyise (p : Player.player) : bool =
  if complete p || qidui p then
    let hidden = filter_hidden p in
    let exposed = Exposed_hand.get_tiles (Player.get_exposed p) in
    let hand = hidden @ exposed in
    List.length (is_zi hand) = List.length hand
  else false

let hunyise (p : Player.player) : bool =
  if complete p || qidui p then
    let hidden = filter_hidden p in
    let exposed = Exposed_hand.get_tiles (Player.get_exposed p) in
    let hand = hidden @ exposed in
    let zi_len = List.length (is_zi hand) in
    let shu_len =
      max
        (List.length (is_tong hand))
        (max (List.length (is_wan hand)) (List.length (is_tiao hand)))
    in
    List.length hand = zi_len + shu_len
  else false

let sananke (p : Player.player) : bool =
  if complete p then
    let hidden = filter_hidden p in
    let exposed_blocks =
      Exposed_hand.exposed_hand_count (Player.get_exposed p)
    in
    let needed = 4 - exposed_blocks in
    try_make_kezi hidden needed 3
  else false

let xiaosixi (p : Player.player) : bool =
  if complete p then
    let hidden = filter_hidden p in
    let exposed = Exposed_hand.get_exposed_hand (Player.get_exposed p) in
    let exposed_count = List.length (List.filter is_wind_group exposed) in
    let needed = 3 - exposed_count in
    let rec try_all_pairs = function
      | [] -> false
      | t :: rest ->
          if count_same hidden t >= 2 && is_wind t then
            let remaining = List.sort Tile.compare_tile (remove_n hidden t 2) in
            if try_make_wind_only remaining needed then true
            else
              try_all_pairs
                (List.filter (fun x -> Tile.compare_tile x t <> 0) rest)
          else try_all_pairs rest
    in
    let unique_tiles = List.sort_uniq Tile.compare_tile hidden in
    try_all_pairs unique_tiles
  else false

(** [is_yaojiu tiles] filters the tiles in [tiles] to only include tiles with
    num 1 or 9. *)
let is_yaojiu tiles =
  List.filter
    (fun t ->
      match Tile.get_num t with
      | 1 | 9 -> true
      | _ -> false)
    tiles

let duanyaojiu (p : Player.player) : bool =
  if complete p || qidui p then
    let hidden = filter_hidden p in
    let exposed = Exposed_hand.get_tiles (Player.get_exposed p) in
    let hand = hidden @ exposed in
    List.length (is_yaojiu hand) = 0
  else false

let qingyaojiu (p : Player.player) : bool =
  if complete p || qidui p then
    let hidden = filter_hidden p in
    let exposed = Exposed_hand.get_tiles (Player.get_exposed p) in
    let hand = hidden @ exposed in
    List.length (is_yaojiu hand) = List.length hand
  else false

let hunyaojiu (p : Player.player) : bool =
  if complete p || qidui p then
    let hidden = filter_hidden p in
    let exposed = Exposed_hand.get_tiles (Player.get_exposed p) in
    let hand = hidden @ exposed in
    List.length (is_yaojiu hand) + List.length (is_zi hand) = List.length hand
  else false

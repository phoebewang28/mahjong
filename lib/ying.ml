exception PlayerWin of Player.player

let count_same tiles tile =
  List.length (List.filter (fun t -> Tile.compare_tile t tile = 0) tiles)

let rec remove_n tiles t n : Tile.tile list =
  match (tiles, n) with
  | [], _ -> []
  | _, 0 -> tiles
  | h :: tl, _ -> if h = t then remove_n tl t (n - 1) else h :: remove_n tl t n

let is_shu t =
  match Tile.suit_to_string (Tile.get_tao t) with
  | "Wan" | "Tong" | "Tiao" -> true
  | _ -> false

let rec remove_tiles tiles ts =
  match ts with
  | [] -> tiles
  | r :: rs -> remove_tiles (remove_n tiles r 1) rs

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

let complete (p : Player.player) : bool =
  let hidden = Hidden_hand.get_tiles (Player.get_hidden p) in
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

let has_pon_or_kan (p : Player.player) : bool =
  List.exists
    (fun g ->
      match Tile.group_to_string g with
      | "San" | "Si" -> true
      | _ -> false)
    (Exposed_hand.get_groups (Player.get_exposed p))

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
      let hidden = Hidden_hand.get_tiles (Player.get_hidden p) in
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

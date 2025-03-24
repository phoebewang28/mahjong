open Mahjong.Exposed_hand

let () =
  let hand = chi_left { num = 1; tao = Tong } [] in
  let hand = chi_middle { num = 2; tao = Tong } hand in
  let hand = chi_right { num = 3; tao = Tong } hand in
  let hand = peng { num = 4; tao = Wan } hand in
  let hand = peng { num = 0; tao = DaPai Zhong } hand in
  let hand = ming_gang { num = 5; tao = Tiao } hand in
  print_endline (exposed_hand_to_string hand)

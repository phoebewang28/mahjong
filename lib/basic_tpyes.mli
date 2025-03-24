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

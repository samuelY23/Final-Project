type card_type =
  | Number
  | Wild
  | Plus2
  | Plus4

exception InvalidMove of string

type t = {
  color : string;
  number : int;
  card_type : card_type;
}

type deck = t list
type players_cards = t list

let get_color_number t = Failure "unimplemented"
let get_cardtype t = Failure "unimplemented"
let check_valid play_card top_card = Failure "unimplemented"
let shuffle deck = Failure "unimplemented"
let distribute deck = Failure "unimplemented"
let play_card players_cards t = Failure "unimplemented"
let pick_one players_cards deck = Failure "unimplemented"
let draw_x t deck players_cards = Failure "unimplemented"
let detect_win player = Failure "unimplemented"
let detect_uno player = Failure "unimplemented"

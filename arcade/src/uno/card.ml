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

let get_color_number t = raise (Failure "Unimplented")
let get_cardtype t = raise (Failure "Unimplented")
let check_valid play_card top_card = raise (Failure "Unimplented")
let shuffle deck = raise (Failure "Unimplented")
let distribute deck = raise (Failure "Unimplented")
let play_card players_cards t = raise (Failure "Unimplented")
let pick_one players_cards deck = raise (Failure "Unimplented")
let draw_x t deck players_cards = raise (Failure "Unimplented")
let detect_win player = raise (Failure "Unimplented")
let detect_uno player = raise (Failure "Unimplented")

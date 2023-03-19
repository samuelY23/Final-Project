type player = {
  cards : Card.players_cards;
  cards_left : int;
}

type t = {
  played_cards : Card.t list;
  top_card : Card.t;
}

let get_top t = Failure "Unimplented"
let next_player t = Failure "Unimplemented"
let next_players t = Failure "Unimplemented"
let reverse t = Failure "Unimplemented"
let get_played_cards t = Failure "Unimplemented"
let get_current_player t = Failure "Unimplemented"

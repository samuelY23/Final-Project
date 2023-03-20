type player = {
  cards : Card.players_cards;
  cards_left : int;
}

type t = {
  played_cards : Card.t list;
  top_card : Card.t;
  current_player : player;
  next_list : player list;
}

let get_top t = raise (Failure "Unimplented")
let next_player t = raise (Failure "Unimplented")
let next_players t = raise (Failure "Unimplented")
let reverse t = raise (Failure "Unimplented")
let get_played_cards t = raise (Failure "Unimplented")
let get_current_player t = raise (Failure "Unimplented")
let skip t = raise (Failure "Unimplented")

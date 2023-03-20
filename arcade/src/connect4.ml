type player = {
  name : string;
  symbol : string;
}

type board = {
  state : string list list;
  game_end : bool;
}

let create_player name symb = raise (Failure "Unimplemented")
let create_board name = raise (Failure "Unimplemented")
let display_board board = raise (Failure "Unimplemented")
let check_win player board = raise (Failure "Unimplemented")
let check_tie board = raise (Failure "Unimplemented")
let make_move player pos = raise (Failure "Unimplemented")
let valid_move pos = raise (Failure "Unimplemented")
let switch_player player = raise (Failure "Unimplemented")
let play_game () = raise (Failure "Unimplemented")

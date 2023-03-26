type player
(* The abstract type of player. *)

type board
(* The abstract type of checkers board. *)

type piece
(* The abstract type of checker pieces. *)

val default_fen : string
val board_init : board
val current_state_fen : board -> string
val current_state_layout : board -> char list
val current_turn : board -> player
val player_score : player -> int
val is_digit : char -> bool
val string_to_list : string -> char list
val make_board : int -> string -> unit
val is_digit : char -> bool
val string_to_list : string -> char list
val new_boardstate : string -> string -> char -> board -> board
val next_piece : char -> char

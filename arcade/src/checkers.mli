type player
(** The abstract type of checkers board. *)

type board
(** The abstract type of checkers board. *)

type piece
(** The abstract type of checker pieces. *)

val default_fen : string
val current_state : string
val current_turn : player
val p1_score : int
val p2_score : int
val is_digit : char -> bool
val string_to_list : string -> string list
val board_init : string -> string
val update_boardstate : string -> string

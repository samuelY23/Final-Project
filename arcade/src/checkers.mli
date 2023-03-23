type player
(** The abstract type of player. *)

type board
(** The abstract type of checkers board. *)

type piece
(** The abstract type of checker pieces. *)

val default_fen : string
val make_board : int -> string -> unit
val is_digit : char -> bool
val string_to_list : string -> char list
val update_boardstate : string -> string

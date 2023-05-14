type player
(** The abstract type representing the player *)

type board
(** The abstract type representing the board *)

val create_player : string -> string -> player
(** [create_player name symb] is the initialization of a player with [name] and
    symbol [symb]. *)

val create_board : unit -> board
(** [create_board ] is the initialization of the standard Connect 4 board. *)

val display_board : board -> unit
(** [display_board board] displays the current game board *)

val check_win : player -> board -> bool
(** [check_win player board] returns true if a player has won the game by
    checking if they have four symbols in a row (vertically, horizontally, or
    diagonally) on the game board. Otherwise, it returns false. *)

val check_tie : board -> bool
(** [check_tie board] returns true if the game has ended in a tie by checking if
    all the spaces on the board are filled. *)

val make_move : player -> int * int -> board -> unit
(** [make_move player pos] takes in a player and a position, and returns the
    game board after the move. *)

val valid_move : int -> board -> int -> bool
(** [valid_move pos] returns whether the current move is valid by checking if
    its space on the board is empty. *)

val switch_player : player -> player -> player -> player
(** [switch_player player] switches players. *)

val play_game : unit -> unit
(** [play_game] is the main loop and runs the game. *)

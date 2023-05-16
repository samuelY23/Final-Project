type player
(** The abstract type of player. *)

type board
(** The abstract type of checkers board. *)

type piece
(** The abstract type of checker pieces. *)

val default_fen : string
(** [deafualt_fen] The initial piece arrangement of the checkers board as a
    Forsyth–Edwards Notation (FEN) string *)
    

val board_init : board
(** [board_init] The initial board state as a board record type containing
    [state] a tuple of fen and layout piece arrangement, [turn] the player whose
    current turn it is and [players] the current players *)

val current_state_fen : board -> string
(** [current_state_fen brd] The fen string of the current board state, given a
    [brd] this evaluates to the fen string of the board layout *)

val current_state_layout : board -> char list
(** [current_state_layout brd] The layout list of the current board state, given
    a [brd] this evaluates to the layout char list of the board layout *)

val current_turn : board -> player
(** [current_turn brd] The current player who's turn it is to play, given a
    [brd] board this evaluates to the player who currently has an active turn *)

val player_score : player -> int
(** [player_score plyr] Given a player [plyr], this evaluates to the player's
    score based on their pieces at the end of a match *)

val make_board : int -> string -> unit
(** [make_board n fen] Given dimensions [n] and fen string [fen] this prints out
    the square board represented by the fen string with the given dimensions *)

val make_move : string -> string -> char -> char list -> char list
(** [make_move start dest piece layout] Evaluates to a new layout char list
    after making a move on the board, [piece] at position [start] in the current
    [layout] is moved to position [dest] *)

val make_capture : string -> string -> char -> char list -> char list
(** [make_capture start dest piece layout] Evaluates to a new layout char list
    after making capturing a piece on the board, [piece] at position [start] in
    the current [layout] is moved to position [dest] if the capture is valid
    with the piece hopped over being absent from the new layout*)

val new_boardstate :
  string ->
  string ->
  char ->
  board ->
  (string -> string -> char -> char list -> char list) ->
  board
(** [new_boardstate start dest piece old_boardstate movetype] Evaluates to new
    board after move or capture is made, passes arguments [start] [dest] [piece]
    into function [movetype] which is either [make_move] or [make_capture] and
    returns what board evaluates to *)

val next_piece : char -> char
(** [next_piece curr] Given [curr] piece valuates to the next piece to take a
    turn after the current turn is over *)

val string_to_list : string -> char list
(**  [string_to_list s] evaluates to a char list of the characters in [s]*)

val string_to_stringlist : string -> string list
(**  [string_to_list s] evaluates to a string list of the characters in [s]*)

val fen_slash_filter : char list -> char list
(** [fen_slash_filter s] evaluates to a char list that contains no '/' characters *)

val list_repeat : char -> int -> char list
(**  [list_repeat s n] evaluates to a list containing [n] entries of the character [s]*)

val fenlist_to_layout : char list -> char list -> char list
(** [fenlist_to_layout layout lst] evaluates to a char list with n number of repeated ' ' characters in place of any numeric character n *)

val is_digit : char -> bool
(**  [is_digit c] True if c is a numeric character else False *)
val replace : char list -> int -> int -> char -> char list
(**  [replace l src sink a] evaluates to layout char list with the character at layout index [src] changed to ' ' and the character at index [sink]  changed to l.(src) *)

val winCheck : char list -> int -> int -> char
(**  [winChech layout xcount ocount] returns the only character that still has pieces on the board, returns ' ' if both characters have piece on the board*)

val join : char list -> string
(**  [join l] evaluates to a string representation of char list [l]*)

val layout_to_fen_helper : string -> char list -> string
(**  [layout_to_fen_helper fen layout] recursively bould [fen] from the characters in [layout] and places a slash in fen every 8 characters, evaluates to fen when layout is empty*)

val to_run_length : char list -> (int * char) list
(** [to_run_length lst] evaluates to an association list of elements (count, char) where count is the number of consecutive occurences of char in lst*)

val pairs_to_string : (int * char) list -> string
(**  [pairs_to_string pairs] evaluetes to a fen string given an association list of elements (count, char) where count is the number of consecutive occurences of char in the return string*)

val repeat : string -> int -> string
(** [repeat s n] evaluates to a string of char s repeated n times*)

val row_builder : char list -> string
(** [row_builder lst_f] returns a string representing a single row in the checkers board giver the row's layout char list [lst_f]*)

val layout_index : string -> int
(** [layout_index pos] returns the list index which is represented by board position [pos], pos is of the form (letter ^ number) where letter is the board column label and number is the board row label e.g. 'a6' 'g8'*)

val on_board : string -> bool
(** [on_board pos] is True if [pos] is a valid position on the board and False otherwise*)

val is_diagonal_adj : string -> string -> char -> int -> bool
(** [is_diagonal_adj pos dest piece stride] is True if [dest] is diagonally reachable in the valid movement direction for the piece [piece] at [pos] given a step size of [stride] away from [pos] *)

val is_valid_move : string list -> char -> char list -> int -> char -> bool
(** [is_valid_move move piece layout stride occupant] is True if the [move] meets all the criteria to be lawfully executed*)

val is_valid_capture : string list -> char -> char list -> bool
(** [is_valid_capture move piece layout] is True if the capture action [move] meets all the criteria to be lawfully executed*)

val is_valid_move_chain : string list -> char list -> bool
(** [is_valid_move_chain move layout] True if [move] is a valid consecutive series of moves*)
  
val make_King : char list -> char list
(** [make_King layout] evaluates to a layout char list with any pawn piece in theopposing home row promoted to a King*)

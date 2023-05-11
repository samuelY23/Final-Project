val string_to_list : string -> char list
val string_to_stringlist : string -> string list
val fen_slash_filter : char list -> char list
val list_repeat : string -> int -> string list
val fenlist_to_layout : char list -> char list -> char list
val is_digit : char -> bool
val replace : char list -> int -> int -> char -> char list
val join : char list -> string
val layout_to_fen_helper : string -> char list -> string
val to_run_length : char list -> (int * char) list
val pairs_to_string : (int * char) list -> string
val repeat : string -> int -> string
val row_builder : char list -> string
val layout_index : string -> int
val on_board : string -> bool
val is_diagonal_adj : string -> string -> char -> int -> bool
val is_valid_move : string list -> char -> char list -> int -> char -> bool
val is_valid_capture : string list -> char -> char list -> bool
val is_valid_move_chain : string list -> char list -> bool

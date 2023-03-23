type player =
  | Player_1 of int
  | Player_2 of int

type board = {
  state : string;
  turn : player;
  players : player list;
}

type piece =
  | White of char
  | Black of char

let default_fen = {|X1X1X1X1/1X1X1X1X/X1X1X1X1/8/8/1o1o1o1o/o1o1o1o1/1o1o1o1o|}

let is_digit = function
  | '1' .. '9' -> true
  | _ -> false

let string_to_list s = List.init (String.length s) (String.get s)

let rec repeat s n =
  match n with
  | 0 -> ""
  | m -> s ^ repeat s (m - 1)

let rec row_builder lst_f =
  match lst_f with
  | [] -> "|"
  | s :: t ->
      let raw_char = String.make 1 s in
      (if is_digit s then repeat "|   " (int_of_string raw_char)
      else "| " ^ raw_char ^ " ")
      ^ row_builder t

let rec make_board n fen =
  let fen_state = String.split_on_char '/' fen in
  match n with
  | 0 ->
      print_endline
        "  +---+---+---+---+---+---+---+---+\n    a   b   c   d   e   f   g   h"
  | f ->
      print_endline
        ("  +---+---+---+---+---+---+---+---+\n" ^ string_of_int n ^ " "
        ^ row_builder (string_to_list (List.nth fen_state (8 - n))));
      make_board (n - 1) fen

(*------------------------------------*)
(*State*)
let board_init =
  {
    state = default_fen;
    turn = Player_1 0;
    players = [ Player_1 0; Player_2 0 ];
  }

let update_boardstate s = raise (Failure "Unimplemented")

(** let current_state: string, undefined for now *)

(**let p2_score: int undefined for now *)

(**let p1_score: int undefined for now *)

(**let current_turn :string undefined for now*)

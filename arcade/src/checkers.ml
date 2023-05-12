open Util

type player =
  | Player_1 of int
  | Player_2 of int

type board = {
  state : string * char list;
  turn : player;
  players : player list;
}

type piece =
  | White of char
  | Black of char

let default_fen = {|X1X1X1X1/1X1X1X1X/X1X1X1X1/8/8/1O1O1O1O/O1O1O1O1/1O1O1O1O|}

let default_layout =
  Util.string_to_list default_fen |> fen_slash_filter |> fenlist_to_layout []

(* ------------------------------------------------------------*)
(*layout conversion*)

(* ------------------------------------------------------------*)

let rec make_board n fen =
  let fen_state = String.split_on_char '/' fen in
  match n with
  | 0 ->
      print_endline
        "  +---+---+---+---+---+---+---+---+\n    a   b   c   d   e   f   g   h"
  | f ->
      print_endline
        ("  +---+---+---+---+---+---+---+---+\n"
        ^ string_of_int (9 - n)
        ^ " "
        ^ Util.row_builder (Util.string_to_list (List.nth fen_state (8 - n))));
      make_board (n - 1) fen

(*------------------------------------*)
(*State*)
let board_init =
  {
    state = (default_fen, default_layout);
    turn = Player_1 0;
    players = [ Player_1 0; Player_2 0 ];
  }

(*------------------------------------*)
(*Movement*)

let make_move start dest piece layout =
  let start_idx = Util.layout_index start in
  let dest_idx = Util.layout_index dest in
  Util.replace layout start_idx dest_idx piece

let make_capture start dest piece layout =
  let lstart = Char.code (String.get start 0) in
  let nstart = int_of_string (String.sub start 1 1) in
  let ldest = Char.code (String.get dest 0) in
  let ndest = int_of_string (String.sub dest 1 1) in
  let captured_pos =
    String.make 1 ((lstart + ldest) / 2 |> Char.chr)
    ^ string_of_int ((nstart + ndest) / 2)
  in
  print_endline captured_pos;
  let aftermove = make_move start dest piece layout in
  let captured_idx = Util.layout_index captured_pos in
  Util.replace aftermove captured_idx (-1) piece

let layout_to_fen layout =
  let fen = Util.layout_to_fen_helper "" layout in
  String.sub fen 0 (String.length fen - 1)

(* let current_state = raise (Failure "Unimplemented") let current_turn = raise
   (Failure "Unimplemented") let p1_score = raise (Failure "Unimplemented") let
   p2_score = raise (Failure "Unimplemented") let update_boardstate = raise
   (Failure "Unimplemented") *)
let new_boardstate start dest piece old_boardstate
    (movetype : string -> string -> char -> char list -> char list) =
  let old_layout =
    match old_boardstate.state with
    | _, b -> b
  in
  let new_layout = movetype start dest piece old_layout in
  {
    state = (new_layout |> layout_to_fen, new_layout);
    turn =
      (match old_boardstate.turn with
      | Player_1 s -> Player_2 0
      | Player_2 b -> Player_1 0);
    players = [ Player_1 0; Player_2 0 ];
  }

let current_state_fen brd =
  match brd.state with
  | a, _ -> a

let current_state_layout brd =
  match brd.state with
  | _, b -> b

let player_score plyr =
  match plyr with
  | Player_1 s -> s
  | Player_2 m -> m

let current_turn brd = brd.turn

let next_piece curr =
  if curr = 'X' then 'O' else if curr = 'O' then 'X' else ' '

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

let default_fen = {|X1X1X1X1/1X1X1X1X/X1X1X1X1/8/8/1o1o1o1o/o1o1o1o1/1o1o1o1o|}

let default_layout =
  [
    'X';
    ' ';
    'X';
    ' ';
    'X';
    ' ';
    'X';
    ' ';
    ' ';
    'X';
    ' ';
    'X';
    ' ';
    'X';
    ' ';
    'X';
    'X';
    ' ';
    'X';
    ' ';
    'X';
    ' ';
    'X';
    ' ';
    ' ';
    ' ';
    ' ';
    ' ';
    ' ';
    ' ';
    ' ';
    ' ';
    ' ';
    ' ';
    ' ';
    ' ';
    ' ';
    ' ';
    ' ';
    ' ';
    ' ';
    'O';
    ' ';
    'O';
    ' ';
    'O';
    ' ';
    'O';
    'O';
    ' ';
    'O';
    ' ';
    'O';
    ' ';
    'O';
    ' ';
    ' ';
    'O';
    ' ';
    'O';
    ' ';
    'O';
    ' ';
    'O';
  ]

let is_digit = function
  | '1' .. '9' -> true
  | _ -> false

(* ------------------------------------------------------------*)
(*layout conversion*)
let string_to_list s = List.init (String.length s) (String.get s)

let string_to_stringlist s =
  List.map (fun x -> String.make 1 x) (string_to_list s)

let rec fen_slash_filter (s : char list) =
  match s with
  | [] -> []
  | h :: t -> if h = '/' then fen_slash_filter t else [ h ] @ fen_slash_filter t

let rec list_repeat s n =
  match n with
  | 0 -> []
  | m -> s :: list_repeat s (m - 1)

let rec fenlist_to_layout layout lst =
  match lst with
  | [] -> layout
  | h :: t ->
      let string_h = String.make 1 h in
      if is_digit h then
        fenlist_to_layout (layout @ list_repeat ' ' (int_of_string string_h)) t
      else fenlist_to_layout (layout @ [ h ]) t
(* ------------------------------------------------------------*)

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
    state = (default_fen, default_layout);
    turn = Player_1 0;
    players = [ Player_1 0; Player_2 0 ];
  }



(*------------------------------------*)
(*Movement*)
let layout_index pos =
  (8 * (int_of_string (String.make 1 (String.get pos 1)) - 1))
  + (int_of_char (String.get pos 0) - Char.code 'a')

let replace l src sink a =
  List.mapi
    (fun i x ->
      match i with
      | i -> if i = src then "" else if i = sink then a else x)
    l

let make_move start dest piece layout =
  let start_idx = layout_index start in
  let dest_idx = layout_index dest in
  replace layout start_idx dest_idx piece

let join l = List.filter (fun s -> s <> "") l |> String.concat ""

let rec to_run_length (lst : char list) : (int * char) list =
  match lst with
  | [] -> []
  | h :: t -> (
      match to_run_length t with
      | (n, c) :: tail when h = c -> (n + 1, h) :: tail
      | tail -> (1, h) :: tail)

let rec pairs_to_string pairs =
  match pairs with
  | [] -> ""
  | (n, l) :: h ->
      (if l = ' ' then string_of_int n else repeat (String.make 1 l) n)
      ^ pairs_to_string h

let rec layout_to_fen fen layout =
  match join layout with
  | "" -> String.sub fen 0 64
  | s ->
      layout_to_fen
        (fen
        ^ (String.sub s 0 8 |> string_to_list |> to_run_length
         |> pairs_to_string)
        ^ "/")
        (string_to_stringlist (String.sub s 8 (String.length s - 8)))


 (* let current_state = raise (Failure "Unimplemented") let current_turn = raise
   (Failure "Unimplemented") let p1_score = raise (Failure "Unimplemented") let
   p2_score = raise (Failure "Unimplemented") let update_boardstate = raise
   (Failure "Unimplemented")  *)
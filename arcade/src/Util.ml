(* takes in a pos [a7] and return an int index in layout*)
let is_digit = function
  | '1' .. '9' -> true
  | _ -> false

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

let join l = String.of_seq (List.to_seq l)

let rec to_run_length (lst : char list) : (int * char) list =
  match lst with
  | [] -> []
  | h :: t -> (
      match to_run_length t with
      | (n, c) :: tail when h = c -> (n + 1, h) :: tail
      | tail -> (1, h) :: tail)

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

let rec pairs_to_string pairs =
  match pairs with
  | [] -> ""
  | (n, l) :: h ->
      (if l = ' ' then string_of_int n else repeat (String.make 1 l) n)
      ^ pairs_to_string h

let rec layout_to_fen_helper fen layout =
  match layout with
  | [] -> fen
  | s ->
      let w = join s in
      layout_to_fen_helper
        (fen
        ^ (String.sub w 0 8 |> string_to_list |> to_run_length
         |> pairs_to_string)
        ^ "/")
        (if String.length w = 8 then []
        else string_to_list (String.sub w 8 ((w |> String.length) - 8)))

let replace l src sink a =
  List.mapi
    (fun i x ->
      match i with
      | i -> if i = src then ' ' else if i = sink then a else x)
    l

let layout_index pos =
  (8 * (int_of_string (String.make 1 (String.get pos 1)) - 1))
  + (int_of_char (String.get pos 0) - Char.code 'a')

let on_board (pos : string) =
  let s = pos in
  let lettercode = Char.code (String.get s 0) in
  let number = int_of_string (String.sub s 1 1) in
  Char.code 'a' <= lettercode
  && Char.code 'h' >= lettercode
  && 8 >= number && 1 <= number

(* converts letter and number to a string of [letternumber], e.g 'a' 6 -> "a6"*)
let lettnum_to_pos (letter : char) (number : int) =
  String.make 1 letter ^ string_of_int number

let is_diagonal_adj (pos : string) (dest : string) (piece : char) (stride : int)
    =
  let s = pos in
  let lettercode = Char.code (String.get s 0) in
  let number = int_of_string (String.sub s 1 1) in
  let down = number + stride in
  let up = number - stride in
  let left = Char.chr (lettercode - stride) in
  let right = Char.chr (lettercode + stride) in
  let reachable =
    if piece = 'X' then (lettnum_to_pos left down, lettnum_to_pos right down)
    else (lettnum_to_pos left up, lettnum_to_pos right up)
  in
  dest = fst reachable || dest = snd reachable

let is_valid_move (move : string list) piece layout (stride : int) =
  let src = List.nth move 0 in
  let sink = List.nth move 1 in

  on_board src && on_board sink
  && is_diagonal_adj src sink piece stride
  && List.nth layout (layout_index sink) = ' '

let rec pair_of_moves moves pairs =
  match moves with
  | h :: t :: m -> pair_of_moves (t :: m) (pairs @ [ [ h; t ] ])
  | [ h ] -> pairs
  | [] -> pairs

(* let is_valid_chain_capture (move : string list) piece layout = true *)
let is_valid_move_chain (move : string list) layout = true

let is_valid_capture move piece layout =
  let s = List.nth move 0 in
  let lettercode = Char.code (String.get s 0) in
  let number = int_of_string (String.sub s 1 1) in
  is_valid_move move piece layout 2 && is_valid_move_chain move layout

(* let chain = pair_of_moves move [] in match chain with | [] -> | h :: t ->
   is_valid_move h piece layout 2 *)

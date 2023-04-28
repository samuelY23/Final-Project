(* takes in a pos [a7] and return an int index in layout*)
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

let is_diagonal_adj (pos : string) (dest : string) (piece : char) =
  let s = pos in
  let lettercode = Char.code (String.get s 0) in
  let number = int_of_string (String.sub s 1 1) in
  let down = number + 1 in
  let up = number - 1 in
  let left = Char.chr (lettercode - 1) in
  let right = Char.chr (lettercode + 1) in
  let reachable =
    if piece = 'X' then (lettnum_to_pos left down, lettnum_to_pos right down)
    else (lettnum_to_pos left up, lettnum_to_pos right up)
  in
  dest = fst reachable || dest = snd reachable

let is_valid_move (move : string list) piece layout =
  let src = List.nth move 0 in
  let sink = List.nth move 1 in
  on_board src && on_board sink
  && is_diagonal_adj src sink piece
  && List.nth layout (layout_index sink) = ' '

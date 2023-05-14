type player = {
  name : string;
  symbol : string;
}

type board = string array array

let rec horizontal_check_win_helper board =
  let rows = Array.length board in
  let cols = Array.length board.(0) in

  let rec consecutive_checker count r c =
    if count = 4 then true
    else if c >= cols then false
    else if c > 0 && board.(r).(c) = board.(r).(c - 1) then
      consecutive_checker (count + 1) r (c + 1)
    else consecutive_checker 1 r (c + 1)
  in
  let rec row_checker r =
    if r >= rows then false
    else
      let bool_check = consecutive_checker 1 r 0 in
      if bool_check then true else row_checker (r + 1)
  in
  row_checker 0

let rec vertical_check_win_helper board =
  let rows = Array.length board in
  let cols = Array.length board.(0) in

  let rec consecutive_checker2 count r c =
    if count = 4 then true
    else if r >= rows then false
    else if r > 0 && board.(r).(c) = board.(r - 1).(c) then
      consecutive_checker2 (count + 1) (r + 1) c
    else consecutive_checker2 1 (r + 1) c
  in
  let rec col_checker c =
    if c >= cols then false
    else
      let bool_check2 = consecutive_checker2 1 0 c in
      if bool_check2 then true else col_checker (c + 1)
  in
  col_checker 0

let rec diagonal_check_win_helper board =
  let rows = Array.length board in
  let cols = Array.length board.(0) in

  let rec diagonal_checker1 count r c =
    if count = 4 then true
    else if r >= rows || c >= cols then false
    else if r > 0 && c > 0 && board.(r).(c) = board.(r - 1).(c - 1) then
      diagonal_checker1 (count + 1) (r + 1) (c + 1)
    else diagonal_checker1 1 (r + 1) (c + 1)
  in

  let rec diagonal_checker2 count r c =
    if count = 4 then true
    else if r >= rows || c < 0 then false
    else if r > 0 && c < cols - 1 && board.(r).(c) = board.(r - 1).(c + 1) then
      diagonal_checker2 (count + 1) (r + 1) (c - 1)
    else diagonal_checker2 1 (r + 1) (c - 1)
  in

  let rec diagonal_win_checker1 r c =
    if c >= cols then false
    else if diagonal_checker1 1 r c || diagonal_win_checker1 r (c + 1) then true
    else diagonal_win_checker1 r (c + 1)
  in

  let rec diagonal_win_checker2 r c =
    if c < 0 then false
    else if diagonal_checker2 1 r c || diagonal_win_checker2 r (c - 1) then true
    else diagonal_win_checker2 r (c - 1)
  in

  let rec row_checker r =
    if r >= rows then false
    else
      let bool_check1 = diagonal_win_checker1 r 0 in
      let bool_check2 = diagonal_win_checker2 r (cols - 1) in
      if bool_check1 || bool_check2 then true else row_checker (r + 1)
  in

  row_checker 0

let create_player name symb : player = { name; symbol = symb }
let create_board name : board = Array.make_matrix 6 7 " "

let display_board (board : board) =
  Array.iter
    (fun row ->
      Array.iter
        (fun col ->
          print_string col;
          print_string "|")
        row;
      print_endline "")
    board

let check_win player board =
  let condition1 = horizontal_check_win_helper board in
  let condition2 = vertical_check_win_helper board in
  let condition3 = diagonal_check_win_helper board in
  condition1 || condition2 || condition3

let check_tie board =
  let height = Array.length board in
  let width = Array.length board.(0) in
  let rec row_empty_space_checker r x =
    if r >= width then true
    else if board.(r).(x) = " " then false
    else row_empty_space_checker r (x + 1)
  in

  let rec full_board_empty_space_checker row =
    if row >= height then true
    else if not (row_empty_space_checker row 0) then false
    else full_board_empty_space_checker (row + 1)
  in
  full_board_empty_space_checker 0

let make_move player pos = raise (Failure "Unimplemented")
let valid_move pos = raise (Failure "Unimplemented")
let switch_player player = raise (Failure "Unimplemented")
let play_game () = raise (Failure "Unimplemented")

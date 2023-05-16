type player = {
  name : string;
  symbol : string;
}

let get_name player = player.name
let get_symbol player = player.symbol
let player_ai = { name = "AI"; symbol = "A" }

type board = string array array

let winner_retriever winner =
  match winner with
  | Some x -> x
  | _ -> failwith ""

let rec horizontal_check_win_helper board =
  let rows = Array.length board in
  let cols = Array.length board.(0) in

  let rec consecutive_checker count r c =
    if count = 4 then true
    else if c >= cols then false
    else if c > 0 && board.(r).(c) = board.(r).(c - 1) && board.(r).(c) != " "
    then consecutive_checker (count + 1) r (c + 1)
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
    else if r > 0 && board.(r).(c) = board.(r - 1).(c) && board.(r).(c) != " "
    then consecutive_checker2 (count + 1) (r + 1) c
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
    else if
      r > 0 && c > 0
      && board.(r).(c) = board.(r - 1).(c - 1)
      && board.(r).(c) != " "
    then diagonal_checker1 (count + 1) (r + 1) (c + 1)
    else diagonal_checker1 1 (r + 1) (c + 1)
  in

  let rec diagonal_checker2 count r c =
    if count = 4 then true
    else if r >= rows || c < 0 then false
    else if
      r > 0
      && c < cols - 1
      && board.(r).(c) = board.(r - 1).(c + 1)
      && board.(r).(c) != " "
    then diagonal_checker2 (count + 1) (r + 1) (c - 1)
    else diagonal_checker2 1 (r + 1) (c - 1)
  in

  let rec diagonal_win_checker1 r c =
    if c >= cols then false
    else if
      diagonal_checker1 1 r c
      || (diagonal_win_checker1 r (c + 1) && board.(r).(c) != " ")
    then true
    else diagonal_win_checker1 r (c + 1)
  in

  let rec diagonal_win_checker2 r c =
    if c < 0 then false
    else if
      diagonal_checker2 1 r c
      || (diagonal_win_checker2 r (c - 1) && board.(r).(c) != " ")
    then true
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
          print_string "|";
          print_string col)
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

let idx = ref 1

let rec valid_move pos board height =
  if pos < 0 || pos > 7 then false
  else if height <= 0 then false
  else if height >= Array.length board then false
  else if board.(height).(pos) = " " then (
    idx := height;
    true)
  else valid_move pos board (height - 1)

let make_move (player : player) pos board =
  let r = fst pos in
  let c = snd pos in
  board.(r).(c) <- player.symbol

let make_move_ai player board height =
  let rand_col = Random.int 6 in
  let move =
    if valid_move rand_col board height then
      board.(!idx).(rand_col) <- player.symbol
    else ()
  in
  move;
  ()

let switch_player current_player player1 player2 =
  if current_player = player1 then player2 else player1

let play_game () =
  let player1 = create_player "Player 1" "R" in
  let player2 = create_player "Player 2" "Y" in
  let game_board = create_board () in
  let current_player = ref player1 in
  let game_end = ref false in
  let winner = ref None in

  while not !game_end do
    display_board game_board;
    print_endline (!current_player.name ^ "'s turn.");
    print_string "Enter column number (1, 6) to make a move: \n";
    print_string "> ";
    let col = read_int () in

    if valid_move (col - 1) game_board 5 then (
      make_move !current_player (!idx, col - 1) game_board;
      if check_win !current_player game_board then (
        display_board game_board;
        winner := Some !current_player;
        game_end := true;
        print_endline ((winner_retriever !winner).name ^ " has won the game. ");
        exit 0)
      else if check_tie game_board then (
        display_board game_board;
        game_end := true;
        print_endline "The game has ended in a tie.";
        exit 0)
      else current_player := switch_player !current_player player1 player2)
    else print_endline "Invalid Input. Try again"
  done

let play_game_ai () =
  let player1 = create_player "Player 1" "R" in
  let game_board = create_board () in
  let current_player = ref player1 in
  let game_end = ref false in
  let winner = ref None in

  while not !game_end do
    display_board game_board;
    if !current_player = player1 then (
      print_endline (!current_player.name ^ "'s turn.");
      print_string "Enter column number (1, 6) to make a move: \n";
      print_string "> ";
      let col = read_int () in

      if valid_move (col - 1) game_board 5 then (
        make_move !current_player (!idx, col - 1) game_board;
        if check_win !current_player game_board then (
          display_board game_board;
          winner := Some !current_player;
          game_end := true;
          print_endline ((winner_retriever !winner).name ^ " has won the game. ");
          exit 0)
        else if check_tie game_board then (
          display_board game_board;
          game_end := true;
          print_endline "The game has ended in a tie.";
          exit 0)
        else current_player := switch_player !current_player player1 player_ai)
      else print_endline "Invalid Input. Try again")
    else (
      print_endline "AI's Turn";
      make_move_ai player_ai game_board 5;
      if check_win player_ai game_board then (
        display_board game_board;
        winner := Some player_ai;
        game_end := true;
        print_endline "The AI has won the game.")
      else if check_tie game_board then (
        display_board game_board;
        game_end := true;
        print_endline "The game ended as a draw. ")
      else current_player := switch_player !current_player player_ai player1)
  done

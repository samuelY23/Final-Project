open OUnit2
open Game
open Account
open Checkers

let same_elements lst1 lst2 =
  let sorted1 = List.sort compare lst1 in
  let sorted2 = List.sort compare lst2 in
  sorted1 = sorted2

(** tesing helpers *)
let isdigit_test (name : string) (char1 : char) expected_output : test =
  name >:: fun _ ->
  assert_equal ~printer:string_of_bool expected_output (Util.is_digit char1)

let next_piece_test (name : string) (char1 : char) expected_output : test =
  name >:: fun _ ->
  assert_equal ~printer:(String.make 1) expected_output
    (Checkers.next_piece char1)

let string_to_list_test (name : string) (s : string) expected_output : test =
  name >:: fun _ -> assert_equal expected_output (Util.string_to_list s)

let string_to_stringlist_test (name : string) (s : string) expected_output :
    test =
  name >:: fun _ -> assert_equal expected_output (Util.string_to_stringlist s)

let fenslashfilter_test (name : string) (s : char list) expected_output : test =
  name >:: fun _ -> assert_equal expected_output (Util.fen_slash_filter s)

let listrepeat_test (name : string) (s : char) (i : int) expected_output : test
    =
  name >:: fun _ -> assert_equal expected_output (Util.list_repeat s i)

let replace_test (name : string) (s : char list) (i : int) (j : int) (k : char)
    expected_output : test =
  name >:: fun _ -> assert_equal expected_output (Util.replace s i j k)

let layoutindex_test (name : string) (k : string) expected_output : test =
  name >:: fun _ -> assert_equal expected_output (Util.layout_index k)

let join_test (name : string) (k : char list) expected_output : test =
  name >:: fun _ -> assert_equal expected_output (Util.join k)

let onboard_test (name : string) (k : string) expected_output : test =
  name >:: fun _ -> assert_equal expected_output (Util.on_board k)

let chess_test =
  [
    isdigit_test "testing a, non digit" 'a' false;
    isdigit_test "testing digit" '1' true;
    isdigit_test "char  " '~' false;
    (* next_piece_test "starting w X " 'X' 'O'; next_piece_test "starting w O "
       'O' 'X'; *)
    next_piece_test "anything else " '`' ' ';
    string_to_list_test "some strings" "abcdefg"
      [ 'a'; 'b'; 'c'; 'd'; 'e'; 'f'; 'g' ];
    string_to_list_test "some strings" "" [];
    string_to_stringlist_test "some strings" "abcdefg"
      [ "a"; "b"; "c"; "d"; "e"; "f"; "g" ];
    fenslashfilter_test "empty" [] [];
    fenslashfilter_test "not empty"
      [ 'a'; 'b'; '/'; 'd'; '/'; '/' ]
      [ 'a'; 'b'; 'd' ];
    listrepeat_test "empty" ' ' 0 [];
    listrepeat_test "empty" 'a' 5 [ 'a'; 'a'; 'a'; 'a'; 'a' ];
    replace_test "empty" [] 0 1 'k' [];
    replace_test "replace" [ 'a'; 'b'; 'c'; 'd' ] 1 3 'f' [ 'a'; ' '; 'c'; 'f' ];
    layoutindex_test "a1 to 0" "a1" 0;
    layoutindex_test "a6 to 40" "a6" 40;
    layoutindex_test "h8 to 63" "h8" 63;
    join_test "empty" [] "";
    join_test "empty2" [ ' '; ' ' ] "  ";
    join_test "not empty" [ 'a'; 'b'; 'c' ] "abc";
    onboard_test "not onboard" "a9" false;
    onboard_test "onboard" "a0" false;
    onboard_test "onboard" "a1" true;
    onboard_test "onboard" "h8" true;
    onboard_test "onboard" "h7" true;
  ]

(** TESTING ACCOUNT *)
let get_name_test (name : string) (acc : Account.t) expected_output : test =
  name >:: fun _ ->
  assert_equal (*~printer: *) expected_output (Account.get_name acc)

let get_balance_test (name : string) (acc : Account.t) expected_output : test =
  name >:: fun _ ->
  assert_equal ~printer:string_of_int expected_output (Account.balance acc)

let add_test (name : string) (amt : int) (acc : Account.t) expected_output :
    test =
  name >:: fun _ ->
  assert_equal expected_output (Account.balance (Account.add amt acc))

let deduct_test (name : string) (amt : int) (acc : Account.t) expected_output :
    test =
  name >:: fun _ ->
  assert_equal expected_output
    (Account.balance (Account.deduct amt acc))
    ~printer:string_of_int

let prod_d_test (name : string) (num : int) (amt : int) (lis : int list)
    expected_output : test =
  name >:: fun _ ->
  assert_equal ~cmp:same_elements expected_output (Account.prob_d num amt lis)

let prod_lst_test (name : string) expected_output : test =
  name >:: fun _ -> assert_equal expected_output Account.prob_lst

let suff_lst_test (name : string) (x : int) (acc : Account.t) expected_output :
    test =
  name >:: fun _ -> assert_equal expected_output (Account.sufficient x acc)

let acc1 = Account.account "success"
let acc2 = Account.account "samuel"
let acc2_new = Account.add 100 acc2

(** Testing CONNECT4, test for all things that dont return type unit*)
let create_player_test (name : string) (x : string) (y : string) expected_output
    : test =
  name >:: fun _ ->
  assert_equal expected_output (Connect4.get_name (Connect4.create_player x y))

let create_player_test2 (name : string) (x : string) (y : string)
    expected_output : test =
  name >:: fun _ ->
  assert_equal expected_output
    (Connect4.get_symbol (Connect4.create_player x y))

let create_board_test (name : string) expected_output : test =
  name >:: fun _ -> assert_equal expected_output (Connect4.create_board ())

let check_win_test (name : string) (player : Connect4.player)
    (board : Connect4.board) expected_output : test =
  name >:: fun _ ->
  assert_equal expected_output (Connect4.check_win player board)

let check_tie_test (name : string) (board : Connect4.board) expected_output :
    test =
  name >:: fun _ -> assert_equal expected_output (Connect4.check_tie board)

let valid_test (name : string) (x : int) (board : Connect4.board) (y : int)
    expected_output : test =
  name >:: fun _ -> assert_equal expected_output (Connect4.valid_move x board y)

let switch_player_test (name : string) (player : Connect4.player)
    (p1 : Connect4.player) (p2 : Connect4.player) expected_output : test =
  name >:: fun _ ->
  assert_equal expected_output (Connect4.switch_player player p1 p2)

let player1 = Connect4.create_player "Samuel" "S"
let player2 = Connect4.create_player "Bob" "B"
let board1 = Connect4.create_board ()

let create_winning_board () =
  let board = Connect4.create_board () in
  (* Set up the winning configuration for player1 *)
  Connect4.make_move player1 (0, 0) board;
  Connect4.make_move player1 (0, 1) board;
  Connect4.make_move player1 (0, 2) board;
  Connect4.make_move player1 (0, 3) board;
  board

let board2 = create_winning_board ()

let create_tied_board () =
  let board = Connect4.create_board () in

  Connect4.make_move player1 (0, 0) board;
  Connect4.make_move player2 (0, 1) board;
  Connect4.make_move player1 (0, 2) board;
  Connect4.make_move player2 (0, 3) board;
  Connect4.make_move player1 (0, 4) board;
  Connect4.make_move player2 (0, 5) board;

  Connect4.make_move player2 (1, 0) board;
  Connect4.make_move player1 (1, 1) board;
  Connect4.make_move player2 (1, 2) board;
  Connect4.make_move player1 (1, 3) board;
  Connect4.make_move player2 (1, 4) board;
  Connect4.make_move player1 (1, 5) board;

  Connect4.make_move player1 (2, 0) board;
  Connect4.make_move player2 (2, 1) board;
  Connect4.make_move player1 (2, 2) board;
  Connect4.make_move player2 (2, 3) board;
  Connect4.make_move player1 (2, 4) board;
  Connect4.make_move player2 (2, 5) board;

  Connect4.make_move player2 (3, 0) board;
  Connect4.make_move player1 (3, 1) board;
  Connect4.make_move player2 (3, 2) board;
  Connect4.make_move player1 (3, 3) board;
  Connect4.make_move player2 (3, 4) board;
  Connect4.make_move player1 (3, 5) board;

  Connect4.make_move player1 (4, 0) board;
  Connect4.make_move player2 (4, 1) board;
  Connect4.make_move player1 (4, 2) board;
  Connect4.make_move player2 (4, 3) board;
  Connect4.make_move player1 (4, 4) board;
  Connect4.make_move player2 (4, 5) board;

  Connect4.make_move player2 (5, 0) board;
  Connect4.make_move player1 (5, 1) board;
  Connect4.make_move player2 (5, 2) board;
  Connect4.make_move player1 (5, 3) board;
  Connect4.make_move player2 (5, 4) board;
  Connect4.make_move player1 (5, 5) board;

  board

let board3 = create_tied_board ()

let create_tied_board2 () =
  let board = Connect4.create_board () in

  Connect4.make_move player1 (0, 0) board;
  Connect4.make_move player2 (0, 1) board;
  Connect4.make_move player1 (0, 2) board;
  Connect4.make_move player2 (0, 3) board;
  Connect4.make_move player1 (0, 4) board;
  Connect4.make_move player2 (0, 5) board;

  Connect4.make_move player2 (1, 0) board;
  Connect4.make_move player1 (1, 1) board;
  Connect4.make_move player2 (1, 2) board;
  Connect4.make_move player1 (1, 3) board;
  Connect4.make_move player2 (1, 4) board;
  Connect4.make_move player1 (1, 5) board;

  board

let board4 = create_tied_board2 ()

let connect_four =
  [
    switch_player_test "play1-> play2" player1 player1 player2 player2;
    switch_player_test "play2-> play1" player2 player1 player2 player1;
    create_player_test "player1" "cat" "c" "cat";
    create_player_test "empty name " "" "c" "";
    create_player_test2 "player1" "cat" "c" "c";
    create_player_test2 "player1 empty character " "cat" "" "";
    check_win_test "no win detected" player1 board2 true;
    check_win_test "no win detected" player1 board1 false;
    check_tie_test "tied" board3 false;
    check_tie_test "no tie" board1 false;
    valid_test "1" 1 board1 2 true;
    valid_test "1" 2 board1 2 true;
    valid_test "failing " 6 board1 6 false;
    valid_test "failing " 5 board1 6 false;
    valid_test "failing" 5 board3 6 false;
    valid_test "failing" 1 board3 2 false;
    valid_test "failing" 0 board3 6 false;
    valid_test "failing" 5 board3 7 false;
    valid_test "failing" 5 board4 6 false;
    valid_test "failing" 1 board4 2 true;
    valid_test "failing" 0 board4 6 false;
    valid_test "failing" 5 board4 7 false;
  ]

let arcade_test =
  [
    get_name_test "kemba" acc2 "samuel";
    get_name_test "get name" acc1 "success";
    get_balance_test "get balance" acc1 10;
    prod_lst_test "testing shuffled lst"
      [
        20;
        0;
        10;
        0;
        0;
        100;
        50;
        0;
        0;
        20;
        0;
        0;
        20;
        0;
        10;
        10;
        0;
        0;
        10;
        0;
        50;
        20;
        100;
        0;
        0;
        50;
        0;
        100;
        20;
        10;
        100;
        0;
        20;
        10;
        0;
        0;
        0;
        10;
        10;
        10;
        10;
        20;
        10;
        0;
        10;
        50;
        0;
        0;
        0;
        10;
        20;
        0;
        10;
        0;
        50;
        0;
        0;
        0;
        50;
        0;
        0;
        20;
        0;
        10;
        0;
        0;
        50;
        0;
        0;
        10;
        50;
        20;
        10;
        10;
        10;
        20;
        0;
        0;
        0;
        50;
        20;
        0;
        0;
        0;
        0;
        50;
        0;
        0;
        0;
        0;
        20;
        100;
        20;
        10;
        0;
        0;
        20;
        0;
        0;
        0;
      ];
    prod_d_test "30 lst" 30 10 []
      [
        10;
        10;
        10;
        10;
        10;
        10;
        10;
        10;
        10;
        10;
        10;
        10;
        10;
        10;
        10;
        10;
        10;
        10;
        10;
        10;
        10;
        10;
        10;
        10;
        10;
        10;
        10;
        10;
        10;
        10;
      ];
    prod_d_test "30 lst" 0 30 [] [];
    suff_lst_test "given list" 11 acc1 false;
    suff_lst_test "should pass" 12 acc2 false;
    suff_lst_test "given list" 11 acc2_new true;
    add_test "adding 10 to acc" 10 acc1 20;
    deduct_test "subtract 10" 90 acc2_new 20;
    get_balance_test "acc2" acc2 10;
    get_balance_test "acc1" acc1 10;
    get_balance_test "acc2_new" acc2_new 110;
  ]

let suite =
  "testing our chess game and arcade style"
  >::: List.flatten [ chess_test; arcade_test; connect_four ]

let _ = run_test_tt_main suite

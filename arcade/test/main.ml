open OUnit2
open Game
open Account
open Checkers
open Util

let same_elements lst1 lst2 =
  let sorted1 = List.sort compare lst1 in
  let sorted2 = List.sort compare lst2 in
  sorted1 = sorted2

(** tesing helpers *)
let isdigit_test (name : string) (char1 : char) expected_output : test =
  name >:: fun _ ->
  assert_equal ~printer:string_of_bool expected_output (Util.is_digit char1)

let next_piece_test (name : string) (char1 : char) expected_output : test =
  name >:: fun _ -> assert_equal expected_output (Checkers.next_piece char1)

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

let isdiagadj_test (name : string) (pos : string) (dest : string) (piece : char)
    (stride : int) expected_output : test =
  name >:: fun _ ->
  assert_equal expected_output (Util.is_diagonal_adj pos dest piece stride)

let str1 = {|X1X1X1X1/1X1X1X1X/X1X1X1X1/8/8/8/8/8|}
let str2 = {|8/8/8/8/8/1O1O1O1O/O1O1O1O1/1O1O1O1O|}

let newstr1 =
  str1 |> Util.string_to_list |> fen_slash_filter |> fenlist_to_layout []

let newstr2 =
  str2 |> Util.string_to_list |> fen_slash_filter |> fenlist_to_layout []

let wincheck_test (name : string) (k : char list) expected_output : test =
  name >:: fun _ -> assert_equal expected_output (Util.winCheck k 0 0)

let chess_test =
  [
    isdigit_test "testing a, non digit" 'a' false;
    isdigit_test "testing digit" '1' true;
    isdigit_test "char  " '~' false;
    next_piece_test "starting w X " 'X' 'O';
    next_piece_test "starting w O " 'O' 'X';
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
    wincheck_test "X wins" newstr1 'X';
    wincheck_test "O wins" newstr2 'O';
    isdiagadj_test "false" "e3" "e3" 'X' 1 false;
    isdiagadj_test "true" "e3" "d4" 'X' 1 true;
    isdiagadj_test "false" "e3" "d4" 'X' 2 false;
    isdiagadj_test "true" "d2" "f4" 'X' 2 true;
    isdiagadj_test "false" "e7" "f8" 'O' 2 false;
    isdiagadj_test "false" "e7" "f6" 'O' 2 false;
    isdiagadj_test "true" "d8" "a5" 'O' 3 true;
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
  >::: List.flatten [ chess_test; arcade_test ]

let _ = run_test_tt_main suite

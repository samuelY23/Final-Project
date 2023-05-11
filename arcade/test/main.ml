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
  assert_equal ~printer:string_of_bool expected_output (Checkers.is_digit char1)

let next_piece_test (name : string) (char1 : char) expected_output : test =
  name >:: fun _ -> assert_equal expected_output (Checkers.next_piece char1)

let string_to_list_test (name : string) (s : string) expected_output : test =
  name >:: fun _ -> assert_equal expected_output (Checkers.string_to_list s)

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
  assert_equal expected_output (Account.balance (Account.deduct amt acc))

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
    get_balance_test "get balance" acc1 0;
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
    add_test "adding 10 to acc" 10 acc1 10;
    deduct_test "subtract 10" 90 acc2_new 10;
    get_balance_test "acc2" acc2 0;
    get_balance_test "acc1" acc1 0;
    get_balance_test "acc2_new" acc2_new 100;
  ]

let suite =
  "testing our chess game and arcade style"
  >::: List.flatten [ chess_test; arcade_test ]

let _ = run_test_tt_main suite

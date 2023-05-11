open OUnit2
open Game
open Account
open Checkers

(** tesing helpers *)
let isdigit_test (name : string) (char1 : char)
    expected_output : test =
  name >:: fun _ ->
  assert_equal ~printer: (string_of_bool)
    expected_output ( Checkers.is_digit char1)

let next_piece_test (name: string) (char1: char) 
      expected_output : test =
      name >:: fun _ ->
        assert_equal 
        expected_output ( Checkers.next_piece char1)
 let string_to_list_test (name: string) (s: string) 
        expected_output : test =
        name >:: fun _ ->
          assert_equal 
          expected_output ( Checkers.string_to_list s)



let chess_test = [
  isdigit_test "testing a, non digit" 'a' false;
  isdigit_test "testing digit" '1' true; 
  isdigit_test "char  " '~' false;
  next_piece_test "starting w X " 'X' 'O';
  next_piece_test "starting w O " 'O' 'X';
  next_piece_test "anything else " '`' ' ';
  string_to_list_test "some strings" "abcdefg" ['a';'b';'c';'d';'e';'f';'g'];


]


let arcade_test = [

] 




let suite = 
  "testing our chess game and arcade style"
  >::: List.flatten [chess_test; arcade_test]
let _ = run_test_tt_main suite 
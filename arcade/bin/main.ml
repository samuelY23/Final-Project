open Game
open Util
open Checkers
open Command
open Account

type 'a player_account = {
  mutable data : 'a option array;
  mutable size : int;
}

let player_accounts : 'a player_account = { data = Array.make 2 None; size = 0 }
let data_dir_prefix = "data" ^ Filename.dir_sep

(* let rec checkers_gameloop (board : Checkers.board) win piece = match win with
   | true -> () | false -> print_string ("\n player " ^ String.make 1 piece ^
   "'s turn"); let start = print_string "\n>"; read_line () in let dest =
   print_string "\n>"; read_line () in let new_piece = Checkers.next_piece piece
   in let board_aftermove = Checkers.new_boardstate start dest piece board in
   Checkers.(board_aftermove |> current_state_fen |> make_board 8);
   checkers_gameloop board_aftermove false new_piece *)

let rec checkers_gameloop (board : Checkers.board) winPiece piece =
  let didWin = if winPiece = ' ' then false else true in
  match didWin with
  | true -> print_endline (String.make 1 winPiece ^ " wins")
  | false -> (
      print_string ("\n player " ^ String.make 1 piece ^ "'s turn");
      let move_input =
        print_string "\n>";
        read_line ()
      in
      match
        Command.parse move_input (Checkers.current_state_layout board) piece
      with
      | move -> (
          match move with
          | Move t ->
              let start = List.nth t 0 in
              let dest = List.nth t 1 in
              let new_piece = Checkers.next_piece piece in
              let board_aftermove =
                Checkers.new_boardstate start dest piece board
                  Checkers.make_move
              in
              Checkers.(board_aftermove |> current_state_fen |> make_board 8);
              checkers_gameloop board_aftermove
                (Util.winCheck
                   (Checkers.current_state_layout board_aftermove)
                   0 0)
                new_piece
          | Capture t ->
              let start = List.nth t 0 in
              let dest = List.nth t 1 in
              let new_piece = Checkers.next_piece piece in
              let board_aftermove =
                Checkers.new_boardstate start dest piece board
                  Checkers.make_capture
              in
              Checkers.(board_aftermove |> current_state_fen |> make_board 8);
              checkers_gameloop board_aftermove
                (Util.winCheck
                   (Checkers.current_state_layout board_aftermove)
                   0 0)
                new_piece
          | Forfeit -> failwith "")
      | exception e -> (
          Command.(
            match e with
            | InvalidInput ->
                print_string
                  "\n\
                   Please format your input property.\n\
                   Use 'move' 'capture' or 'forfeit'\n";
                checkers_gameloop board ' ' piece
            | InvalidMove ->
                print_string
                  "\n\
                   Piece can't move like that, please check your coordinates\n";
                checkers_gameloop board ' ' piece
            | _ ->
                print_string "";
                checkers_gameloop board ' ' piece)))

(** [account_retriever acct] returns an account *)
let account_retriever acct =
  match acct with
  | None -> failwith "No account"
  | Some x -> x

let change_amount amt acc = Account.add amt acc

(** [create_account name] creates an account for a user with an initial amount
    of $0 *)
let create_account name =
  let account = Account.account name in
  player_accounts.data.(player_accounts.size) <- Some account;
  player_accounts.size <- player_accounts.size + 1

let print_number_range () =
  print_string "\nPlease pick a number between 1 and 100\n";
  print_string "> "

let congrats_message name =
  print_string
    ("Congrats, " ^ name
   ^ " your account has been created. You have an amount of $0 to start\n")
(* let change_amount_in_arr number_pick player_accounts idx =
   player_accounts.data.(0) <- Some ((Account.add (Account.get_init_amount
   number_pick) (account_retriever (player_accounts.data.(idx))))) *)

(** [play_game f] starts the adventure in file [f]. *)
let play_game f = raise (Failure "Unimplemented: Main.play_game")

(** [main ()] prompts for the game to play, then starts it. *)
let main () =
  print_string "\n\nWelcome to our Arcade!!\n Be Competitive and Have Fun!!\n";
  print_string "\n\nHow many players will be playing?\n";
  print_string "> ";

  match read_line () with
  | x -> (
      if x = "1" then (
        print_string "\n\nPlease enter your name\n";
        print_string "> ";
        let name = read_line () in
        create_account name;
        congrats_message name;
        print_number_range ();
        let number_pick = int_of_string (read_line ()) in
        let new_account =
          Account.add
            (Account.get_init_amount number_pick)
            (account_retriever player_accounts.data.(0))
        in
        player_accounts.data.(0) <- Some new_account;
        print_string
          ("Congrats, you have $"
          ^ string_of_int (Account.balance new_account)
          ^ "\n"))
      else if x = "2" then (
        print_string "\n\nPlayer 1, please enter your name\n";
        let name = read_line () in
        let () =
          print_string "";
          create_account name;
          congrats_message name;
          print_number_range ();
          let number_pick = int_of_string (read_line ()) in
          let new_account =
            Account.add
              (Account.get_init_amount number_pick)
              (account_retriever player_accounts.data.(0))
          in
          player_accounts.data.(0) <- Some new_account;
          print_string
            ("Congrats, you have $"
            ^ string_of_int (Account.balance new_account)
            ^ "\n");

          print_string "\n\nPlayer 2, please enter your name\n";
          let name = read_line () in
          create_account name;
          print_number_range ();
          let number_pick2 = int_of_string (read_line ()) in
          let new_account2 =
            Account.add
              (Account.get_init_amount number_pick2)
              (account_retriever player_accounts.data.(1))
          in
          player_accounts.data.(1) <- Some new_account2;
          print_string
            ("Congrats, you have $"
            ^ string_of_int (Account.balance new_account2)
            ^ "\n")
        in
          print_string "")
        else print_string "Enter either 1 or 2";

        (* game_select *)
        print_string "\n\nSelect a game?\n- checkers\n- uno\n- connect4\n>";
        let game_choice = read_line () in
        if game_choice = "checkers" then (
          print_string "\n\nWelcome to checkers, -10pt per player\n";
          print_string "\nPlayer 1 : X\nPlayer 2 : O\n";
          Checkers.(board_init |> current_state_fen |> make_board 8);
          checkers_gameloop Checkers.board_init ' ' 'X')
        else ();

        (* game_select; *)
        match read_line () with
        | exception End_of_file -> ()
        | file_name -> play_game (data_dir_prefix ^ file_name ^ ".json"))



(* game_select *)

(* Execute the game engine. *)
let () = main ()

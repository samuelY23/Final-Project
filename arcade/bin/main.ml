open Game

type 'a player_account = {
  mutable data : 'a option array;
  mutable size : int;
}

let player_accounts : 'a player_account = { data = Array.make 2 None; size = 0 }
let data_dir_prefix = "data" ^ Filename.dir_sep

let rec checkers_gameloop (board : Checkers.board) win piece =
  match win with
  | true -> ()
  | false ->
      print_string ("\n player " ^ String.make 1 piece ^ "'s turn");
      let start =
        print_string "\n>";
        read_line ()
      in
      let dest =
        print_string "\n>";
        read_line ()
      in
      let new_piece = Checkers.next_piece piece in
      let board_aftermove = Checkers.new_boardstate start dest piece board in
      Checkers.(board_aftermove |> current_state_fen |> make_board 8);
      checkers_gameloop board_aftermove false new_piece

(** [create_account name] creates an account for a user with an initial amount
    of $0 *)
let create_account name =
  Account.account name;
  player_accounts.data.(player_accounts.size) <- Some name;
  player_accounts.size <- player_accounts.size + 1

(** [play_game f] starts the adventure in file [f]. *)
let play_game f = raise (Failure "Unimplemented: Main.play_game")

(** [main ()] prompts for the game to play, then starts it. *)
let main () =
  print_string "\n\nWelcome to our Arcade!!\n Be Competitive and Have Fun!!\n";
  print_string "\n\nHow many players will be playing?\n";
  print_string "> ";
  let player_number =
    match read_line () with
    | x -> (
        if x = "1" then (
          print_string "\n\nPlease enter your name\n";
          print_string "> ";
          let name = read_line () in
          let account = create_account name in
          print_string
            ("Congrats, " ^ name
           ^ " your account has been created. You have an amount of $0 to start"
            ))
        else if x = "2" then (
          print_string "\n\nPlayer 1, please enter your name\n";
          let name = read_line () in
          let account = create_account name in
          print_string
            ("Congrats, " ^ name
           ^ " your account has been created. You have an amount of $0 to start\n"
            );

          print_string "\n\nPlayer 2, please enter your name\n";
          let name = read_line () in
          let account = create_account name in
          print_string
            ("Congrats, " ^ name
           ^ " your account has been created. You have an amount of $0 to start"
            ))
        else print_string "Enter either 1 or 2";

        (* game_select *)
        print_string "\n\nSelect a game?\n- checkers\n- uno\n- connect4\n>";
        let game_choice = read_line () in
        if game_choice = "checkers" then (
          print_string "\n\nWelcome to checkers, -10pt per player\n";
          print_string "\nPlayer 1 : X\nPlayer 2 : O\n";
          Checkers.(board_init |> current_state_fen |> make_board 8);
          checkers_gameloop Checkers.board_init false 'X')
        else if game_choice = "uno" then ()
        else ();

        (* game_select; *)
        match read_line () with
        | exception End_of_file -> ()
        | file_name -> play_game (data_dir_prefix ^ file_name ^ ".json"))
  in
  print_string ""

(* game_select *)

(* Execute the game engine. *)
let () = main ()

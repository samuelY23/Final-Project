open Game

type 'a player_account = 
{
  mutable data : 'a option array ;
  mutable size : int;
}

let player_accounts : 'a player_account = {
  data = Array.make 2 None;
  size = 0;
}

(** [create_account name] creates an account for a user with an initial amount
    of $0 *)
let create_account name = Game.Account.account name;
player_accounts.data.(player_accounts.size) <- Some name;
player_accounts.size <- player_accounts.size + 1
(** [play_game f] starts the adventure in file [f]. *)
let play_game f = raise (Failure "Unimplemented: Main.play_game")

let data_dir_prefix = "data" ^ Filename.dir_sep

(** [main ()] prompts for the game to play, then starts it. *)
let main () =
  print_string "\n\nWelcome to our Arcade!!\n Be Competitive and Have Fun!!\n";
  print_string "\n\nHow many players will be playing?\n";
  print_string "> ";
  let player_number = match read_line() with 
  | x -> if x = "1" then (print_string "\n\nPlease enter your name\n";
    print_string "> ";
  let name = read_line () in
  let account = create_account name in
  print_string
    ("Congrats, " ^ name
   ^ " your account has been created. You have an amount of $0 to start")) else (if x = "2" then (print_string "\n\nPlayer 1, please enter your name\n";
    let name = read_line () in
  let account = create_account name in
  print_string
    ("Congrats, " ^ name
   ^ " your account has been created. You have an amount of $0 to start\n");

   print_string "\n\nPlayer 2, please enter your name\n";
    let name = read_line () in
  let account = create_account name in
  print_string
    ("Congrats, " ^ name
   ^ " your account has been created. You have an amount of $0 to start")) else (print_string "Enter either 1 or 2"));
  
  

  match read_line () with
  | exception End_of_file -> ()
  | file_name -> play_game (data_dir_prefix ^ file_name ^ ".json") in print_string "placeholder"

(* Execute the game engine. *)
let () = main ()
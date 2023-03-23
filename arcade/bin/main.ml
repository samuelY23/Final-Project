(** [play_game f] starts the adventure in file [f]. *)
let play_game f = raise (Failure "Unimplemented: Main.play_game")

let data_dir_prefix = "data" ^ Filename.dir_sep

(** [main ()] prompts for the game to play, then starts it. *)
let main () =
  print_string "> ";
  Game.Checkers.make_board 8 Game.Checkers.default_fen

(* Execute the game engine. *)
let () = main ()

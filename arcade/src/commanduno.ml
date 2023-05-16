open Util

type action  = string list


type command =
  | Play of action 
  | Quit 


exception InvalidMove (** breaking a move*)
exception InvalidInput (** bad input , wrong input format or nonexisting cord*)


let parse_helper  lst =
  match lst with
  | [] -> raise InvalidInput
  | h :: t -> (
      match h with
      | "play" -> if (List.length t <> 2)then raise InvalidInput else Play t 
      | "quit" -> if List.length t > 0 then raise InvalidInput else Quit
      | _ -> raise InvalidInput)


let parse str top_card =
  String.split_on_char ' ' str
  |> List.filter (fun x -> x <> "" )
  |> parse_helper   











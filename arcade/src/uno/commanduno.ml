open Util

type action  = string list


type command =
  | Play of action 
  | Quit 


exception InvalidMove (** breaking a move*)
exception InvalidInput (** bad input , wrong input format or nonexisting cord*)


let parse_helper (layout : char list) play_cards  top_card lst =
  match lst with
  | [] -> raise InvalidInput
  | h :: t -> (
      match h with
      | "play" -> if (List.length t <> 2)then raise InvalidInput else if (Unostate.check_valid play_cards top_card ) then Play t else raise InvalidMove
      | "quit" -> if List.length t > 0 then raise InvalidInput else Quit
      | _ -> raise InvalidInput)


let parse str layout play_cards top_card =
  String.split_on_char ' ' str
  |> List.filter (fun x -> x <> "" )
  |> parse_helper layout play_cards top_card 











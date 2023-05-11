open Util

type action  = string list


type command =
  | Move of action 
  | Capture of action  
  | Forfeit


exception InvalidMove (** breaking a move*)
exception InvalidInput (** bad input , wrong input format or nonexisting cord*)


let parse_helper (layout : char list) turn lst =
  match lst with
  | [] -> raise InvalidInput
  | h :: t -> (
      match h with
      | "move" -> if (List.length t < 2)then raise InvalidInput else if (Util.is_valid_move t turn layout 1) then Move t else raise InvalidMove
      | "capture" -> if List.length t < 2 then raise InvalidInput  else if (Util.is_valid_move_chain t layout) then Capture t else raise InvalidMove (** implement this *)
      | "forfeit" -> if List.length t > 0 then raise InvalidInput else Forfeit
      | _ -> raise InvalidInput)


let parse str layout turn =
  String.split_on_char ' ' str
  |> List.filter (fun x -> x <> "" )
  |> parse_helper layout turn 











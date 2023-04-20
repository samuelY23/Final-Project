type action  = string list


type command =
  | Go of action 
  | Capture of action  
  | Forfeit


exception InvalidMove (** breaking a move*)
exception InvalidInput (** bad input , wrong input format or nonexisting cord*)

let is_valid_move = failwith "unimplemented "(****************************************)

let parse_helper lst =
  match lst with
  | [] -> raise InvalidInput
  | h :: t -> (
      match h with
      | "move" -> if List.length t < 2 then raise InvalidInput else Go t
      | "capture" -> if List.length t < 2 then raise InvalidInput  else if is_valid_move then Capture t else raise InvalidMove (** implement this *)
      | "forfeit" -> if List.length t > 0 then raise InvalidInput else Forfeit
      | _ -> raise InvalidInput)


let parse str =
  String.split_on_char ' ' str
  |> List.filter (fun x -> x <> "" )
  |> parse_helper








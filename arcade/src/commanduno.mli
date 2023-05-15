(** Parsing of player commands. *)

type action = string list
(** The type [object_phrase] represents the object phrase that can be part of a
    player command. Each element of the list represents a word of the object
    phrase, where a "word" is defined as a consecutive sequence of non-space
    characters. Thus, no element of the list should contain any leading,
    internal, or trailing spaces. The list is in the same order as the words in
    the original player command. For example:

    - If the player command is ["go A4 A5"], then the object phrase is
      [\["A4"; "A5"\]].

    - If the player command is ["go A4     A5"], then the object phrase is again
      [\["A4"; "A5"\]]. *)

(* Note that the backslashes in the OCamldoc comment above are inserted by
   OCamlformat for sake of the HTML version of the documentation. When reading
   the source code of the comment in this file, pretend that the backslashes do
   not exist. That is, the object phrase is simply [["A4"; "A5"]]. *)

(** The type [command] represents a player command that is decomposed into a
    verb and possibly an object phrase. Invariant: the [action] carried by [Go]
    must not be empty. *)
type command =
  | Play of action
  | Quit

exception InvalidMove
(** Raised when its not a valid move to make like too far, not diagonal*)

exception InvalidInput
(** Raised when bad input , wrong input format or nonexisting cord*)

val parse :
  string -> char list -> Unostate.card_data -> Unostate.card_data -> command
(** [parse str] parses a player's input into a [command], as follows. The first
    word (i.e., consecutive sequence of non-space characters) of [str] becomes
    the verb. The rest of the words, if any, become the object phrase. Examples:

    - [parse "    move A4 A5   "] is [Move \["A4"; "A5"\]]
    - [parse "    capture   A4   A5   "] is [Capture \["A4"; "A5"\]] (**checks
      if is valid capture*)
    - [parse "forfeit"] is [Forfeit].

    Requires: [str] contains only alphanumeric (A-Z, a-z, 0-9) and space
    characters (only ASCII character code 32; not tabs or newlines, etc.).

    Raises: [InvalidMove] if [str] is not a valid move to make in the Checkers
    game.

    Raises: [InvalidInput] if the command is malformed. A command is malformed
    if the verb is neither "forfeit" nor "go", or if the verb is "forfeit" and
    there is a non-empty object phrase, or if the verb is "go" and there is an
    empty object phrase.*)

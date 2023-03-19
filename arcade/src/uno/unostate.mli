type t

type player = {
  cards : Card.players_cards;
  cards_left : int;
}

val get_top : t -> Card.t
(** [get_top t] gets the top card of the played cards*)

val get_played_cards : t -> Card.t list
(** [get_played_cards t] returns a list of the cards that have been played *)

val get_current_player : t -> player
(** [get_current_player t] return the current player*)

val next_player : t -> player
(** [next_player t] returns the next player given current state *)

val next_players : t -> player list
(** [next_players t] returns a list of next players given current state *)

val reverse : t -> player list
(** [reverse t] reverses the current next players given some uses a reverse car
    list current state *)
val skip : t -> player list
    (** [skip t] skips the next player *)
  


type color
type card_type
type card_data
type player

exception InvalidMove

type t
(** abstract type t,represents the card data, THIS IS FOR THE GROUP WILL DELETE
    THIS SPEC LATER*)

type deck = card_data list
(** general deck of cards*)

type players_cards = card_data list
(** set of cards distributed to a player*)

val create_player : players_cards -> player
val create_initstate : card_data -> player -> player -> t

val get_top : t -> card_data
(** [get_top t] gets the top card of the played cards*)

val get_played_cards : t -> card_data list
(** [get_played_cards t] returns a list of the cards that have been played *)

val get_current_player : t -> player
(** [get_current_player t] return the current player*)

val next_player : t -> player
(** [next_player t] returns the next player given current state *)

val next_players : t -> player list
(** [next_players t] returns a list of next players given current state *)

val reverse : t -> t
(** [reverse t] reverses the current next players given some uses a reverse car
    list current state *)

val skip : t -> t
(** [skip t] skips the next player *)

(** CARD STUFFFFFF *)
val get_color_number : card_data -> color * int
(**[get_color_number t] takes in type t and return the color and number as a
   tuple*)

val get_cardtype : card_data -> card_type
(**[get_get_cardtype t] takes in type t and return the card type*)

(** [check_valid t] checks whether the card is a valid card to play based on
    current card*)
val check_valid : card_data -> card_data -> bool
(** may change the second argument to take in game state rather than card*)

val shuffle : deck -> deck
(** [shuffle deck] scrambles order of cards in the deck *)

val distribute : deck -> players_cards list * deck
(** [distribute deck] distributes the deck players, and gives then their own
    cards *)

val play_card : card_data -> t -> t
(** [play_card players_cards t] allows player to play/use a specific card in
    their deck returns their updated cards*)

val pick_one : players_cards -> deck -> int -> players_cards
(** [pick_one players_cards deck] allows player to pick a card from the deck if
    they dont have any playable cards or just choose to, return the card they
    picked up *)

val draw_x : card_type -> deck -> players_cards -> players_cards
(** [draw_x t players_cards deck] takes in the card type, if it is a draw_2 or
    draw_4 then it gives the player their respected amount of cards *)

val detect_win : player -> bool
(** [detect_win player] takes in player and sees weather they can win *)

val detect_uno : player -> bool
(** [detect_uno player] takes in player and see weather they have uno, which is
    they have one card left *)

val shuffled : deck
val cards_to_string : card_data list -> string
val get_players_cards : player -> players_cards

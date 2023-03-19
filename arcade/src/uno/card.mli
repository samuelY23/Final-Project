open Unostate

type card_type =
  | Number
  | Wild
  | Plus2
  | Plus4

exception InvalidMove

type t
(** abstract type t,represents the card data, THIS IS FOR THE GROUP WILL DELETE
    THIS SPEC LATER*)

type deck = t list
(** general deck of cards*)

type players_cards = t list
(** set of cards distributed to a player*)

val get_color_number : t -> string * int
(**[get_color_number t] takes in type t and return the color and number as a
   tuple*)

val get_cardtype : t -> card_type
(**[get_get_cardtype t] takes in type t and return the card type*)

(** [check_valid t] checks whether the card is a valid card to play based on
    current card*)
val check_valid : t -> t -> bool
(** may change the second argument to take in game state rather than card*)

val shuffle : deck -> deck
(** [shuffle deck] scrambles order of cards in the deck *)

val distribute : deck -> players_cards list list
(** [distribute deck] distributes the deck players, and gives then their own
    cards *)

val play_card : players_cards -> t -> players_cards
(** [play_card players_cards t] allows player to play/use a specific card in
    their deck returns their updated cards*)

val pick_one : players_cards -> deck -> t
(** [pick_one players_cards deck] allows player to pick a card from the deck if
    they dont have any playable cards or just choose to, return the card they
    picked up *)

val draw_x : t -> deck -> players_cards -> players_cards
(** [draw_x t players_cards deck] takes in the card type, if it is a draw_2 or
    draw_4 then it gives the player their respected amount of cards *)

val detect_win : Unostate.player -> bool
(** [detect_win player] takes in player and sees weather they can win *)

val detect_uno : Unostate.player -> bool
(** [detect_uno player] takes in player and see weather they have uno, which is
    they have one card left *)

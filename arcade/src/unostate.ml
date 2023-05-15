type color =
  | Red
  | Blue
  | Green
  | Yellow

type card_type =
  | Number
  | Wild
  | Plus2
  | Plus4
  | Reverse
  | Skip

type card_data = {
  color : color;
  number : int;
  card_type : card_type;
}

type deck = card_data list
type players_cards = card_data list

type player = {
  cards : players_cards;
  cards_left : int;
}

type t = {
  played_cards : card_data list;
  top_card : card_data;
  current_player : player;
  next_list : player list;
}

exception InvalidMove

let get_top t = t.top_card
let next_player t = List.nth t.next_list 0
let next_players t = t.next_list

let reverse t =
  {
    played_cards = t.played_cards;
    top_card = t.top_card;
    current_player = t.current_player;
    next_list = List.rev t.next_list;
  }

let get_played_cards t = t.played_cards
let get_current_player t = t.current_player

let skip t =
  {
    played_cards = t.played_cards;
    top_card = t.top_card;
    current_player = t.current_player;
    next_list =
      (match t.next_list with
      | [] -> failwith "not possible"
      | h :: t -> t @ [ h ]);
  }

let get_color_number t = (t.color, t.number)
let get_cardtype t = t.card_type

let check_valid play_card top_card =
  let top_data = get_color_number top_card in
  let played = get_color_number play_card in
  match (played, top_data) with
  | (fst, snd), (thrd, four) -> if fst = thrd || snd = four then true else false

let shuffle (deck : deck) : deck =
  let compare_random _ _ = Random.int 3 - 1 in
  List.sort compare_random deck

let rec split_at n lst =
  match (n, lst) with
  | 0, _ -> ([], lst)
  | _, [] -> ([], [])
  | n, x :: xs ->
      let prefix, suffix = split_at (n - 1) xs in
      (x :: prefix, suffix)

let distribute (deck : deck) : players_cards list =
  let shuffled_deck = shuffle deck in

  let rec distribute_aux players_cards remaining_deck num_cards =
    if num_cards = 0 then List.rev players_cards
    else
      let player_cards, rest_deck = split_at 7 remaining_deck in
      distribute_aux (player_cards :: players_cards) rest_deck (num_cards - 1)
  in

  distribute_aux [] shuffled_deck (List.length shuffled_deck / 7)

let rec find_remove card_data players_cards =
  match players_cards with
  | [] -> failwith "not existing card"
  | h :: t -> if h == card_data then t else find_remove card_data t

let decresase_card_count card_data (player : player) : player =
  {
    cards_left = player.cards_left - 1;
    cards = find_remove card_data player.cards;
  }

let play_card card_dataa t =
  {
    played_cards = card_dataa :: t.played_cards;
    top_card = card_dataa;
    current_player = next_player t;
    next_list =
      (match t.next_list with
      | [] -> failwith "nobody"
      | h :: t -> t @ [ h ]);
  }

let rec pick_one (players_card : players_cards) (deck : deck) (amount : int) :
    players_cards =
  if amount <> 0 then pick_one players_card deck (amount - 1)
  else List.hd deck :: players_card

let draw_x t deck players_cards =
  match t with
  | Plus2 -> pick_one players_cards deck 2
  | Plus4 -> pick_one players_cards deck 4
  | _ -> failwith "not a pickable card"

let detect_win player = player.cards_left = 0
let detect_uno player = player.cards_left = 1

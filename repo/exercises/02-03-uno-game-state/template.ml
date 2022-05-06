let next_player (g : game) : game = raise NotImplemented

let get_playable_cards (g : game) : card list = raise NotImplemented

let pick_up (g : game) : game * card = raise NotImplemented

let effect_card (c : card) (g : game) : game = raise NotImplemented

let play_card (n : int) (g : game) : game =
  let playable_cards = get_playable_cards g in
  let chosen_card = List.nth playable_cards (n - 1) in
  let (current_player_hand :: other_player_hands, deck, discard) = g in
  let current_player_hand = remove_card chosen_card current_player_hand in
  effect_card chosen_card (current_player_hand :: other_player_hands, deck, add_card chosen_card discard)

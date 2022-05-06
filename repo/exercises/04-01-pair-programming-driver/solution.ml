let rec find_all_diamonds hand = match hand with
  | [] -> []
  | card :: cards ->
    if suit_of card = Diamonds then
      card :: find_all_diamonds cards
    else
      find_all_diamonds cards

let is_diamond = fun card -> suit_of card = Diamonds

let rec find_all predicate hand = match hand with
  | [] -> []
  | card :: cards ->
    if predicate card then
      card :: find_all predicate cards
    else
      find_all predicate cards

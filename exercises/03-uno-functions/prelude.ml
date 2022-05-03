type color = Red | Blue | Yellow | Green 

type value = Numeric of int | Skip | Plus2 | Reverse

type card = color * value

type hand = card list

type player = hand
type deck = hand
type discard = hand

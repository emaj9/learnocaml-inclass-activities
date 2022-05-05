exception NotImplemented

type suit = Hearts | Diamonds | Spades | Clubs
type rank =
  | Num of int (* only numbers from 2 to 9 allowed! *)
  | Jack
  | Queen
  | King
  | Ace
type card = rank * suit

type hand = card list

(* Although we can of course just use fst and snd directly,
   it's nice to define more meaningful aliases for working with cards. *)
let suit_of : card -> suit = snd
let rank_of : card -> rank = fst

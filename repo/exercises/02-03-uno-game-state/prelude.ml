(* Solution to part 1: *)

type color = Red | Blue | Yellow | Green 

type value = Numeric of int | Skip | Plus2 | Reverse

type card = color * value

type hand = card list

type player = hand
type deck = hand
type discard = hand

(* Solution to part 2: *)

let can_follow (cl1 , v1) (cl2 , v2) = 
  cl1 = cl2 || v1 = v2

(* Helper function to determine valid moves! *)
let rec find_follow_ups hand card = 
  match hand with
  | head::tail -> 
    if can_follow card head 
      then head :: (find_follow_ups tail card) else find_follow_ups tail card
  | [] -> []

let rec remove_card card hand = 
  match hand with 
  | head::tail -> if card=head then tail else head :: (remove_card card tail)
  (* QUESTION TO STUDENTS
    what do we do with the empty list? meaning we didn't find the card
    throw error? return empty list? use an option? return a bool (and a tuple :(..) ?*)
  | [] -> []

let add_card card pile = card :: pile

(* PART 3: The game state *)

type game = (player list, deck, discard)

(* The state of the game is encoded as a triple.
   1. The list of players, being ordered, encodes the turn order of the players.
      The current player is understood as the player at the head of this
      list.
   2. The deck of cards available to draw.
   3. The discard pile, which has the invariant of never being empty.
 *)

(* SHUFFLING *)

(* split_at n l = (l1, l2) such that
   l1 @ l2 = l
   List.length l1 = minimum of n or List.length l
 *)
let rec split_at i deck = match (i, deck) with
  | (0, deck) -> ([], deck)
  | (i, card :: deck) ->
     let (front, back) = split_at (i-1) deck in
     (card :: front, back)

(* cuts a deck in two at a random point *)
let random_split deck =
  let i = Random.int (List.length deck) in
  split_at i deck

let join (front, back) = back @ front

(* interleave l1 l2 = l such that
   l contains values from l1 at even indices
   l contains values from l2 at odd indices
   assuming that List.length l1 = List.length l2
   Otherwise, the elements alternate until one list is exhausted and all
   subsequent elements come from the other list.
 *)
let rec interleave front back = match (front, back) with
  | (x :: front, y :: back) -> x :: y :: interleave front back
  | (front, []) -> front
  | ([], back) -> back

(* Cuts a deck down the middle and interleaves the cards of the resulting
   half-decks
 *)
let riffle deck =
  let (front, back) = split_at (List.length deck / 2) deck in
  interleave front back

(* Repeats `riffle` n times on the given deck. *)
let riffle_times n deck = match n with
  | 0 -> deck
  | n -> riffle_times (n - 1) (riffle deck)

(* Shuffles a deck by randomly splitting and joining it a few times, then
   riffling it ten times.
 *)
let shuffle deck =
  deck |> random_split |> join |> random_split |> join |> random_split |> join |> riffle_times 10

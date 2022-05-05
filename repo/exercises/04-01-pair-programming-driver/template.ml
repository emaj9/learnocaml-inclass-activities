(* This is a hand you can mess with in the toplevel for testing your
   implementations *)
let sample_hand = [
  (Queen, Spades);
  (Num 2, Clubs);
  (Num 7, Clubs);
  (Num 7, Diamonds);
  (Jack, Hearts);
]

let find_all_diamonds (hand : hand) : hand = raise TODO

let filter p = function
  | [] -> []
  | x :: xs -> if p x then x :: filter p xs else filter p xs

let find_all_diamonds hand = match hand with
  | [] -> []
  | card :: cards ->
    if suit_of card = Diamond then
      card :: find_all_diamonds cards
    else
      find_all_diamonds cards

(* Given to both students:
   - Definitions:
     * type suit = ...
     * type rank = ...
     * type card = suit * rank
   - header for `find_all_diamonds` i.e.
     let rec find_all_diamonds (hand : hand) : hand = ???
   Exercise:
   - student A implements find_all_diamonds with guidance from B.
   - B has the solution, and some guiding questions
   - A thinks about how to generalize find_all_diamonds into find_all
     which selects all cards satisfying a predicate
   - key observation: the part of the function that needs to be "swapped out" to
     achieve the different selection behaviours is the condition of the `if`
     expression. The condition needs to depend on the element.
 *)


let filter_map

let concat_map

let for_all

let exists

let find_opt


(* TODO
   Read description on the right and fill in the blanks! *)
type color = Red | _ | _ | _ 

type value = Numeric of int | Skip | _ | _ 

type card = (_ * _)

type hand = _ list

type player = hand
type _ = _ 
type _ = _ 

(*=================================================*)
(* Functions 
   Please do not add type annontations to these functions for auto-grading reasons!*)
let can_follow (color1, value1) (color2, value2) = 
   _ || _ 

let rec find_follow_ups hand card = 
   raise NotImplemented

let rec remove_card card hand = 
   raise NotImplemented

let rec add_card card pile = 
   raise NotImplemented
(* Trace the execution of: sum [1, 2, 3] *)
(* For the purposes of this exercise, we will consider only the steps right
   before recursive calls, so need need to write out the steps involving
   match-expressions. To clarify, consider th *)

let sum_step_1 = sum [1;2;3]
let sum_step_2 = raise NotImplemented
(* add more steps until you reach a value *)

let sum_tr_step_1 = sum_tr 0 [1;2;3]
let sum_tr_step_2 = raise NotImplemented
(* add more steps until you reach a value *)

let sum_step_1 = sum [1;2;3]
let sum_step_2 = 1 + sum [2;3]
let sum_step_3 = 1 + (2 + sum [3])
let sum_step_4 = 1 + (2 + (3 + sum []))
let sum_step_5 = 1 + (2 + (3 + 0))
let sum_step_6 = 1 + (2 + 3)
let sum_step_7 = 1 + 5
let sum_step_8 = 6

let sum_tr_step_1 = sum_tr 0 [1;2;3]
let sum_tr_step_2 = sum_tr (0 + 1) [2;3]
let sum_tr_step_3 = sum_tr (1 + 2) [3]
let sum_tr_step_4 = sum_tr (3 + 3) []
let sum_tr_step_5 = 6

let rec sum l = match l with
  | [] -> 0
  | x :: xs -> x + sum xs

let rec sum_tr acc l = match l with
  | [] -> acc
  | x :: xs -> sum_tr (acc + x) xs

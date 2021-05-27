let sum = fun x -> x + 1 in

let rec sum = 
  fun x -> 
    if x <= 0 then 0 else (x + (sum (x-1))) in

sum 10

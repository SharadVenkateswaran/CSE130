let add = fun x -> x + 1 in let rec f = fun x -> if x = 100 then 0 else x + f (add x) in f 0

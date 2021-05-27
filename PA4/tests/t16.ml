let rec fa = fun x -> if x = 100 then 0 else (x + fa (x + 1)) in fa 1

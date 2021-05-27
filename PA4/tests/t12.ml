let foldn = 
  fun f -> fun b -> fun n ->
    let rec loop = 
      fun i -> fun c ->
        if i <= n then (loop (i+1)) ((f i) c) else c in
    (loop 0) b in

let fac = (foldn (fun x -> fun y -> (if x = 0 then 1 else (x * y)))) 1 in

fac 10

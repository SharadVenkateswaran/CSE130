(* CSE 130: Programming Assignment 2
 * misc.ml
 *)

(* ***** DOCUMENT ALL FUNCTIONS YOU WRITE OR COMPLETE ***** *)
(*  assoc: a' * b' * (b' * a') list -> a'
    Find the first (k',v) in l where k' = k, and return v
    If no such k' exists, return d

    Recursively strip tuples from l; if k' matches, set v to the default
    calue and recurse on the empty list to immediately return v *)

let rec assoc (d,k,l) =
    let rec helper (d, k, l) =
    match l with
    | [] -> d
    | (k', v)::t -> if(k' = k) then helper (v, k, []) else helper(d, k, t)
in helper(d, k, l)

(* removeDuplicates: a' list -> a' list
    Return a list which contains all elements of l with the duplicates
    removed
    Strip elements from l; if the element has been seen already, do not add
    it to the list. Otherwise, add it to the list and to seen. *)

let removeDuplicates l = 
  let rec helper (seen,rest) = 
      match rest with 
        [] -> seen
      | h::t -> 
        let seen' = if List.mem h seen then seen else h::seen in
        let rest' = t in
	  helper (seen',rest') 
  in
      List.rev (helper ([],l))

(*  wwhile: ('a -> 'a * bool) * 'a -> 'a
    Call f on b to get (b', c'), then recurisvely call f to update b' until
    c' is false. *)

let rec wwhile (f,b) =
    let rec helper (f, (b',c')) =
    match c' with
    | false -> b'
    | true -> helper (f, (f b'))
in helper(f, (f b))

(*  fixpoint: ('a -> 'a) * 'a -> 'a
    Recursively call (f b) until b = (f b)

    Pass a function to wwhile which returns a tuple containing the output 
    of a call to f on the input x, and a bool for whether or not x is equal
    to the result of that call. *)

let fixpoint (f,b) = wwhile ((fun x -> (f x, x != f x)),b)


(* ffor: int * int * (int -> unit) -> unit
   Applies the function f to all the integers between low and high
   inclusive; the results get thrown away.
 *)

let rec ffor (low,high,f) = 
  if low>high 
  then () 
  else let _ = f low in ffor (low+1,high,f)
      
(************** Add Testing Code Here ***************)

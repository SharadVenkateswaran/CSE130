(* CSE 130: Programming Assignment 3
 * misc.ml
 *)

(* For this assignment, you may use the following library functions:

   List.map
   List.fold_left
   List.fold_right
   List.split
   List.combine
   List.length
   List.append
   List.rev

   See http://caml.inria.fr/pub/docs/manual-ocaml/libref/List.html for
   documentation.
*)



(* Do not change the skeleton code! The point of this assignment is to figure
 * out how the functions can be written this way (using fold). You may only
 * replace the   failwith "to be implemented"   part. *)



(*****************************************************************)
(******************* 1. Warm Up   ********************************)
(*****************************************************************)
(*  sqsum : int list -> int
    Returns the sum of the squares of all digits in the list
    Use List.fold_left to accumulate sums of squares
*)
let sqsum xs = 
  let f a x = a + (x*x) in
  let base = 0 in
    List.fold_left f base xs

(*  pipe: ('a -> 'a) list -> ('a -> 'a)
    Turns a list of functions into a single function which calls all the given functions in sequence
    I still can't believe this one works
*)
let pipe fs = 
  let f a x = fun y -> x (a y) in
  let base =  fun x -> x in
    List.fold_left f base fs

(*  sepConcat: string -> string list -> string
    Takes a delimiter and a list of strings
    Produces a string which is the result of concatenating the strings in the list
    and separating with the delimiter

    List.fold_left accumulates the concatenated string
*)
let rec sepConcat sep sl = match sl with 
  | [] -> ""
  | h :: t -> 
      let f a x =  if a = "" then x else a^sep^x in
      let base = "" in
      let l = sl  in
        List.fold_left f base l

(* stringOfList : ('a -> string) -> 'a list -> string
    Takes a list and a function to convert elements of the list to strings
    Returns a string representation of the list of elements.

    Uses sepConcat to separate elements with "; ", then surrounds with brackets
*)
let stringOfList f l = "["^(sepConcat "; " (List.map f l))^"]"

(*****************************************************************)
(******************* 2. Big Numbers ******************************)
(*****************************************************************)

(*  clone: a -> int -> 'a list
    Produces a list which contains a n times
    Recursively builds the list while decrementing n
*)
let rec clone x n = match n with
| x when x < 1 -> []
| _ -> List.append (clone x (n - 1)) [x] 

(*  padZero: int list -> int list -> int list * int list
    Adds 0s to the beginning of the shorter list until they are the same length
    Recurisvely adds 0s to the shorter list
*)
let rec padZero l1 l2 = match List.length(l1) - List.length(l2) with
| x when x < 0 -> padZero (0::l1) l2
| 0 -> (l1, l2)
| _ -> padZero l1 (0::l2)

(*  removeZero: int list
    Removes 0s from the beginning of the list
    Recursively strips the first element until the first element is not 0
*)
let rec removeZero l = match l with
| 0::t -> removeZero t
| _ -> l

(*  bigAdd: int list -> int list -> int list
    Adds the list representations of two ints
    elements in the lists are digits of the ints, in order
    List.fold_left accumulates the summed digits
    Digits are "carried" within the accumulated list, and stripped off if necessary by removeZero
*)
let bigAdd l1 l2 = 
  let add (l1, l2) = 
    let f a x = match (a, x) with 
    | ((_, h::t), (c,d)) -> (false, [(c+d+h)/10;(c+d+h) mod 10]@t)
    | ((_, []), (c, d)) -> (false, [(c+d)/10;(c+d) mod 10]) in
    let base = (false, []) in
    let args =  List.rev(List.combine l1 l2) in
    let (_, res) = List.fold_left f base args in
      res
  in 
    removeZero (add (padZero l1 l2))

(*  mulByDigit: int -> int list -> int list
    Multiplies the list representation of an int with a digit [0...9]
    Recursively adds the list to itself i - 1 times with bigAdd
*)
let rec mulByDigit i l = match i with
| 0 -> []
| 1 -> l
| _ -> bigAdd l (mulByDigit (i - 1) l)

(*  bigMul: int list -> int list -> int list
    Multiplies the list representations of two integers to create a new list
    
    Passes the first list along with the accumulator
    List.fold_left multiplies digits in the second list with the first list using mulByDigit
    Results are multiplied by the appropriate power of 10, then summed into the accumulated list
*)
let bigMul l1 l2 = 
  let f a x = match (a, x) with
  | (((a, 1), []), c) -> ((a,10), (mulByDigit c a))
  | (((a, x), h::t), c) -> ((a, (x*10)), (bigAdd (h::t) (mulByDigit x (mulByDigit c a)))) 
  | _ -> (([], 0), []) in
  let base = ((l1, 1), []) in
  let args = List.rev(l2) in
  let (_, res) = List.fold_left f base args in
    res

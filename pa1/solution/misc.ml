(* CSE 130: Programming Assignment 1
 * misc.ml
 *)

(* append: 'a list -> 'a list
    append l x builds a new list from l with x "appended" to the end
    Recurses on l down to empty list, then builds back up from [x]
*)

let rec append l =
match l with
| h::t -> fun x -> h::append t x
| [] -> fun x -> [x]

(* sumList : int list -> int 
    sumList l is the sum of all the integers which appear in l
    Recursively strip leading elements from list and sum them together
*) 

let rec sumList l = 
match l with
| [] -> 0
| h::t -> h + sumList t

(* digitsOfInt : int -> int list 
    digitsOfInt i is a list which contains the digits of i in order as 
    elements of an int list
    Recursively extract digits from the right and build a list by 
    "appending" them
   (see the digits function below for an example of what is expected)
*) 

let rec digitsOfInt n = 
match n with
| n when n < 10 -> [n]
| _         -> append (digitsOfInt (n/10)) (n mod 10)

(* digits : int -> int list
 * (digits n) is the list of digits of n in the order in which they appear
 * in n
 * e.g. (digits 31243) is [3,1,2,4,3]
 *      (digits (-23422) is [2,3,4,2,2]
*) 
 
let digits n = digitsOfInt (abs n)


(* From http://mathworld.wolfram.com/AdditivePersistence.html
 * Consider the process of taking a number, adding its digits, 
 * then adding the digits of the number derived from it, etc., 
 * until the remaining number has only one digit. 
 * The number of additions required to obtain a single digit from a number n 
 * is called the additive persistence of n, and the digit obtained is called 
 * the digital root of n.
 * For example, the sequence obtained from the starting number 9876 is (9876, 30, 3), so 
 * 9876 has an additive persistence of 2 and a digital root of 3.
 *)


(* ***** PROVIDE COMMENT BLOCKS FOR THE FOLLOWING FUNCTIONS ***** *)

(* additivePersistence: int -> int
    (additivePersistence n) is defined as above - the number of iterations
    of summing digits of n to reach a single digit number
    Recursively sum digits by breaking ints into lists of digits with
    digitsOfInt, then summing them with sumList
    If n is less than 10, no further iterations necessary, 
    otherwise recurse on the sum of the digits of n
*)

let rec additivePersistence n =
match n with
| n when n < 10 -> 0
| _ -> additivePersistence (sumList (digitsOfInt (n))) + 1

(* digitalRoot: int -> int
    (digitalRoot n) is defined as above - the number which results from 
    repeatedly summing digits of n until a single digit number is reached
    Recursively sum digits by breaking n into list of digits with 
    digitsOfInt, then summing them with sumList
    If n is less than 10, digital root has been reached
    Otherwise, recurse on the sum of the digits of n
*)

let rec digitalRoot n =
match n with
| n when n < 10 -> n
| _ -> digitalRoot (sumList (digitsOfInt (n)))

(* listReverse: 'a list -> 'a list
    (listReverse l) is a list which contains all elements in l in reverse 
    order
    Recursively extract the first element of the list and append it to the
    rest of the list
*)

let rec listReverse l =
match l with
| [] -> []
| h::t -> append (listReverse t) h

(* explode : string -> char list 
 * (explode s) is the list of characters in the string s in the order in 
 *   which they appear
 * e.g.  (explode "Hello") is ['H';'e';'l';'l';'o']
 *)

let explode s = 
  let rec _exp i = 
    if i >= String.length s then [] else (s.[i])::(_exp (i+1)) in
  _exp 0

(* palindrome: string -> bool
    (palindrome w) is true if w is equal to itself reversed (case-sensitive)
    Compare w to its reverse by exploding w and reversing the resulting
    list with listReverse
*)

let palindrome w =
if listReverse (explode w) = explode w then true else false

(************** Add Testing Code Here ***************)


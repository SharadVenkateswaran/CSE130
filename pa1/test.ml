(* CSE 130 PA 1. Autotester *)
#use "misc.ml"

let key = "" (* change *)
let prefix130 = "130" (* change *)
let print130 s = print_string (prefix130^">>"^s)

exception ErrorCode of string

type result = Pass | Fail | ErrorCode of string

let score = ref 0
let max = ref 0
let timeout = 300

(* 
exception TimeOutException
let reset_alarm () = Sys.set_signal Sys.sigalrm (Sys.Signal_ignore)
let set_alarm timeout =
  ignore (Sys.signal
            Sys.sigalrm
            (Sys.Signal_handle (fun i -> reset_alarm (); raise TimeOutException)));
  ignore (Unix.alarm timeout)

let runWTimeout (f,arg,out,time) = 
  let rv = ref (ErrorCode "timeout") in  
  set_alarm timeout;
  let rv = 
    try if compare (f arg) out = 0 then Pass else Fail
    with TimeOutException -> ErrorCode "timeout"
    | e -> 
      (print130 ("Uncaught Exception: "^(Printexc.to_string e));
       ErrorCode "exception") in
  reset_alarm ();
  rv
*)

let runWTimeout (f,arg,out,time) = 
  try if compare (f arg) out = 0 then Pass else Fail
  with e -> (print130 ("Uncaught Exception: "^(Printexc.to_string e)); ErrorCode "exception") 

exception TestException
let testTest () =
  let testGood x = 1 in
  let testBad x = 0 in 
  let testException x = raise TestException in
  let rec testTimeout x = testTimeout x in
    runWTimeout(testGood,0,1,5) = Pass &&  
    runWTimeout(testBad,0,1,5) = Fail &&  
    runWTimeout(testException,0,1,5) = ErrorCode "exception" && 
    runWTimeout(testTimeout,0,1,5) = ErrorCode "timeout"


let runTest (f,arg,out,points,name) =
  let _ = max := !max + points in
  let outs = 
	match runWTimeout(f,arg,out,timeout) with 
	    Pass -> (score := !score + points; "[pass]")
 	  | Fail -> "[fail]"
	  | ErrorCode e -> "[error: "^e^"]"  in
  name^" "^outs^" ("^(string_of_int points)^")\n"

(* explode : string -> char list *)
let explode s = 
  let rec _exp i = 
    if i >= String.length s then [] else (s.[i])::(_exp (i+1)) in
  _exp 0

let implode cs = 
  String.concat "" (List.map (String.make 1) cs)

let drop_paren s = 
  implode (List.filter (fun c -> not (List.mem c ['(';' ';')'])) (explode s))

let eq_real p (r1,r2) = 
  (r1 -. r2) < p || (r2 -. r1) < p

let runAllTests () =
    let _ = (score := 0; max := 0) in
    let report =
      [
      runTest (sumList, [1;2;3;4],10,1,"sumList 1");
      runTest (sumList, [1;-2;3;5],7,1,"sumList 2");
      runTest (sumList, [1;3;5;7;9;11],36,1,"sumList 3");
      runTest (sumList, [0;0;0;0],0,1,"sumList 3");
      runTest (sumList, [1;-1;1;-1;2],2,1,"sumList 3");
      runTest (digitsOfInt,3124,[3;1;2;4],2,"digitsOfInt 1");
      runTest (digitsOfInt,352663,[3;5;2;6;6;3],1,"digitsOfInt 2");
      runTest (digits, 31243,[3;1;2;4;3],1,"digits 1");
      runTest (digits, -23422, [2;3;4;2;2],1,"digits 2");
      runTest (digits, 0, [0],1,"digits 2");
      runTest (digits, 01, [1],1,"digits 2");
      runTest (digits, 12121, [1;2;1;2;1],1,"digits 2");
      runTest (additivePersistence, 9876,2,2,"additivePersistence");
      runTest (additivePersistence, 0000,0,2,"additivePersistence");
      runTest (additivePersistence, 55,2,2,"additivePersistence");
      runTest (additivePersistence, 55555554,3,2,"additivePersistence");
      runTest (digitalRoot, 9876,3,2,"digitalRoot");
      runTest (digitalRoot, 0,0,2,"digitalRoot");
      runTest (digitalRoot, 10,1,2,"digitalRoot");
      runTest (digitalRoot, 121219,7,2,"digitalRoot");
      runTest (listReverse, [1;2;3;4],[4;3;2;1],1,"reverse 1");
      runTest (listReverse, [], [],1,"reverse 2");
      runTest (listReverse, [1;2;1],[1;2;1],1,"reverse 3");
      runTest (listReverse, [1],[1],1,"reverse 4");
      runTest (listReverse, ["a";"b";"a";"b"],["b";"a";"b";"a"],1,"reverse 5");
      runTest (listReverse, ["a";"b";"c";"d"], ["d";"c";"b";"a"],1,"rev 2");
      runTest (palindrome, "malayalam", true, 1, "palindrome 1");
      runTest (palindrome, "", true, 1, "palindrome 2");
      runTest (palindrome, "aa", true, 1, "palindrome 3");
      runTest (palindrome, "aba", true, 1, "palindrome 4");
      runTest (palindrome, "abab", false, 1, "palindrome 4");
      runTest (palindrome, "myxomatosis", false, 1, "palindrome 5")
      ] in
    let s = Printf.sprintf "Results: Score/Max = %d / %d \n" !score !max in
    let _ = List.iter print130 (report@([s])) in
    (!score,!max)

let _ = runAllTests ()

let _ = print130 ("Compiled"^key^"\n")

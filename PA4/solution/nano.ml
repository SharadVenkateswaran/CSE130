exception MLFailure of string

type binop = 
  Plus 
| Minus 
| Mul 
| Div 
| Eq 
| Ne 
| Lt 
| Le 
| And 
| Or          
| Cons

type expr =   
  Const of int 
| True   
| False      
| NilExpr
| Var of string    
| Bin of expr * binop * expr 
| If  of expr * expr * expr
| Let of string * expr * expr 
| App of expr * expr 
| Fun of string * expr    
| Letrec of string * expr * expr
	
type value =  
  Int of int		
| Bool of bool          
| Closure of env * string option * string * expr 
| Nil                    
| Pair of value * value     

and env = (string * value) list

(* binopToString: binop -> string
    Return strings corresponding to binary operators *)
let binopToString op = 
  match op with
      Plus -> "+" 
    | Minus -> "-" 
    | Mul -> "*" 
    | Div -> "/"
    | Eq -> "="
    | Ne -> "!="
    | Lt -> "<"
    | Le -> "<="
    | And -> "&&"
    | Or -> "||"
    | Cons -> "::"

let rec valueToString v = 
  match v with 
    Int i -> 
      Printf.sprintf "%d" i
  | Bool b -> 
      Printf.sprintf "%b" b
  | Closure (evn,fo,x,e) -> 
      let fs = match fo with None -> "Anon" | Some fs -> fs in
      Printf.sprintf "{%s,%s,%s,%s}" (envToString evn) fs x (exprToString e)
  | Pair (v1,v2) -> 
      Printf.sprintf "(%s::%s)" (valueToString v1) (valueToString v2) 
  | Nil -> 
      "[]"

and envToString evn =
  let xs = List.map (fun (x,v) -> Printf.sprintf "%s:%s" x (valueToString v)) evn in
  "["^(String.concat ";" xs)^"]"

and exprToString e =
  match e with
      Const i ->
        Printf.sprintf "%d" i
    | True -> 
        "true" 
    | False -> 
        "false"
    | Var x -> 
        x
    | Bin (e1,op,e2) -> 
        Printf.sprintf "%s %s %s" 
        (exprToString e1) (binopToString op) (exprToString e2)
    | If (e1,e2,e3) -> 
        Printf.sprintf "if %s then %s else %s" 
        (exprToString e1) (exprToString e2) (exprToString e3)
    | Let (x,e1,e2) -> 
        Printf.sprintf "let %s = %s in \n %s" 
        x (exprToString e1) (exprToString e2) 
    | App (e1,e2) -> 
        Printf.sprintf "(%s %s)" (exprToString e1) (exprToString e2)
    | Fun (x,e) -> 
        Printf.sprintf "fun %s -> %s" x (exprToString e) 
    | Letrec (x,e1,e2) -> 
        Printf.sprintf "let rec %s = %s in \n %s" 
        x (exprToString e1) (exprToString e2) 
    | _  -> failwith "should not be reached"
(*********************** Some helpers you might need ***********************)

let rec fold f base args = 
  match args with [] -> base
    | h::t -> fold f (f(base,h)) t

let listAssoc (k,l) = 
  fold (fun (r,(t,v)) -> if r = None && k=t then Some v else r) None l

(*********************** Your code starts here ****************************)

(* lookup: s * (s*value) list -> value
    Return the first value v1 where (x,v1) is in the environment
    If not such v1 exists, raise MLFailures *)
let lookup (x,evn) = match listAssoc(x,evn) with
| None -> raise (MLFailure ("variable not bound: "^x))
| Some(v) -> v

(* binop_helper: binop -> value -> value -> value
    Helper for eval
    Applies the binary operator to the two values
    If the two values are not the same type, raise MLFailures
    If the operator cannot be applied to the values, raise MLFailure *)
let binop_helper op v1 v2 =
match (v1, v2) with
| (Int(x),Int(y)) -> (match op with
    | Plus  -> Int(x + y)
    | Minus -> Int(x - y)
    | Mul   -> Int(x * y)
    | Div   -> Int(x / y)
    | Eq    -> Bool(x = y)
    | Ne    -> Bool(x != y)
    | Lt    -> Bool(x < y)
    | Le    -> Bool(x <= y)
    | _ -> raise (MLFailure("Invalid operation on Ints.")))
| (Bool(x),Bool(y)) -> (match op with
    | Eq    -> Bool(x = y)
    | Ne    -> Bool(x != y)
    | And   -> Bool (x && y)
    | Or    -> Bool (x || y)
    | _ -> raise (MLFailure("Invalid operation on Bools.")))
| _ -> raise (MLFailure ("Cannot operate on Int and Bool."))

(* eval: (string*value) list * expr -> value
    Recursively evaluate e based on environment evn
    Binary operators are applied in binop_helper
    Recursive and non-recursive functions handled in separate
    match cases in App - difference is in what environment is used to
    evaluate. *)
let rec eval (evn,e) = 
match e with
| Bin(x,op,y)     -> binop_helper op (eval (evn, x)) (eval (evn, y))
| Var(s)          -> lookup(s,evn)
| Const(x)        -> Int(x)
| True            -> Bool(true)
| False           -> Bool(false)
| If(a,b,c)       -> (match (eval (evn, a)) with
    | Bool(true)  -> eval (evn, b)
    | Bool(false) -> eval (evn, c)
    | _ -> raise (MLFailure("Conditional statement with non-Bool.")))
| Let(s,x,y)      -> eval(((s,eval(evn,x))::evn),y)
| Letrec(s,x,y)   -> (match x with
    | Fun(a,b)    -> eval((s,Closure(evn, Some(s), a, b))::evn,y)
    | _           -> eval(evn,Let(s,x,y)))
| Fun(s,e)        -> Closure(evn, None, s, e)
| App(e1,e2)      -> (match eval(evn,e1) with
    | Closure(evn1, None, s, e) -> eval((s,eval(evn,e2))::evn1,e)
    | Closure(evn1, Some(x), s, e) -> eval((s,eval(evn,e2))::evn,e)
    | _                      -> raise (MLFailure("Invalid App.")))
| _               -> failwith("Haven't done extra credit")


(**********************     Testing Code  ******************************)

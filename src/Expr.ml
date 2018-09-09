(* Simple expressions: syntax and semantics *)

(* Opening a library for generic programming (https://github.com/dboulytchev/GT).
   The library provides "@type ..." syntax extension and plugins like show, etc.
*)
open GT 
             
(* The type for the expression. Note, in regular OCaml there is no "@type..." 
   notation, it came from GT. 
*)
@type expr =
  (* integer constant *) | Const of int
  (* variable         *) | Var   of string
  (* binary operator  *) | Binop of string * expr * expr with show

(* Available binary operators:
    !!                   --- disjunction
    &&                   --- conjunction
    ==, !=, <=, <, >=, > --- comparisons
    +, -                 --- addition, subtraction
    *, /, %              --- multiplication, division, reminder
*)

(* State: a partial map from variables to integer values. *)
type state = string -> int

(* Empty state: maps every variable into nothing. *)
let empty = fun x -> failwith (Printf.sprintf "Undefined variable %s" x)

(* Update: non-destructively "modifies" the state s by binding the variable x 
   to value v and returns the new state.
*)
let update x v s = fun y -> if x = y then v else s y

(* An example of a non-trivial state: *)                                                   
let some_state = update "x" 1 @@ update "y" 2 @@ update "z" 3 @@ update "t" 4 empty

(* Some testing; comment this definition out when submitting the solution. *)
(* let _ =
  List.iter
    (fun x ->
       try  Printf.printf "%s=%d\n" x @@ s x
       with Failure s -> Printf.printf "%s\n" s
    ) ["x"; "a"; "y"; "z"; "t"; "b"]
*)

(* Casts boolean to int by following rule:
   false -> 0
   true  -> 1
*)
let boolToInt b = if b then 1 else 0

(* Casts int to boolean by following rule:
   0         -> false
   otherwise -> true
*)
let intToBool i = if i != 0 then true else false

(* Evaluate binary operation on arguments.
   val evalBinop : string -> int -> int
*)
let evalBinop op l r = match op with
    | "+"  -> l + r
    | "-"  -> l - r
    | "*"  -> l * r 
    | "/"  -> l / r
    | "%"  -> l mod r
    | "<"  -> boolToInt (l < r)
    | "<=" -> boolToInt (l <= r)
    | ">"  -> boolToInt (l > r)
    | ">=" -> boolToInt (l >= r)
    | "==" -> boolToInt (l == r)
    | "!=" -> boolToInt (l != r)
    | "&&" -> boolToInt (intToBool l && intToBool r)
    | "!!" -> boolToInt (intToBool l || intToBool r)

(* Expression evaluator

     val eval : state -> expr -> int
 
   Takes a state and an expression, and returns the value of the expression in 
   the given state.
*)
let rec eval s e = match e with 
    | Const c          -> c 
    | Var v            -> s v
    | Binop (op, l, r) -> evalBinop op (eval s l) (eval s r)   


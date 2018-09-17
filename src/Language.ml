(* Opening a library for generic programming (https://github.com/dboulytchev/GT).
   The library provides "@type ..." syntax extension and plugins like show, etc.
*)
open GT
open List

(* Opening a library for combinator-based syntax analysis *)
open Ostap.Combinators
       
(* Simple expressions: syntax and semantics *)
module Expr =
  struct
    
    (* The type for expressions. Note, in regular OCaml there is no "@type..." 
       notation, it came from GT. 
    *)
    @type t =
    (* integer constant *) | Const of int
    (* variable         *) | Var   of string
    (* binary operator  *) | Binop of string * t * t with show

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

    (* Expression parser. You can use the following terminals:

         IDENT   --- a non-empty identifier a-zA-Z[a-zA-Z0-9_]* as a string
         DECIMAL --- a decimal constant [0-9]+ as a string
   
    *)
    ostap (
      parse: empty {failwith "Not implemented yet"}
    )

  end
                    
(* Simple statements: syntax and sematics *)
module Stmt =
  struct

    (* The type for statements *)
    @type t =
    (* read into the variable           *) | Read   of string
    (* write the value of an expression *) | Write  of Expr.t
    (* assignment                       *) | Assign of string * Expr.t
    (* composition                      *) | Seq    of t * t with show

    (* The type of configuration: a state, an input stream, an output stream *)
    type config = Expr.state * int list * int list 

    (* Statement evaluator

          val eval : config -> t -> config

       Takes a configuration and a statement, and returns another configuration
    *)
    let rec eval (state, inp, out) stmt = match stmt with
        | Read symb          -> (Expr.update symb (hd inp) state, tl inp, out)
        | Write e            -> (state, inp, out @ [Expr.eval state e]) 
        | Assign (symb, e)   -> (Expr.update symb (Expr.eval state e) state, inp, out)
        | Seq (stmt1, stmt2) -> eval (eval (state, inp, out) stmt1) stmt2

    (* Statement parser *)
    ostap (
      parse: empty {failwith "Not implemented yet"}
    )
      
  end

(* The top-level definitions *)

(* The top-level syntax category is statement *)
type t = Stmt.t    

(* Top-level evaluator

     eval : t -> int list -> int list

   Takes a program and its input stream, and returns the output stream
*)
let eval p i =
  let _, _, o = Stmt.eval (Expr.empty, i, []) p in o

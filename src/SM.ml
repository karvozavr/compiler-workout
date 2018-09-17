open GT     
open List  
open Language
       
(* The type for the stack machine instructions *)
@type insn =
(* binary operator                 *) | BINOP of string
(* put a constant on the stack     *) | CONST of int                 
(* read to stack                   *) | READ
(* write from stack                *) | WRITE
(* load a variable to the stack    *) | LD    of string
(* store a variable from the stack *) | ST    of string with show

(* The type for the stack machine program *)                                                               
type prg = insn list

(* The type for the stack machine configuration: a stack and a configuration from statement
   interpreter
 *)
type config = int list * Stmt.config

(* Stack machine interpreter

     val eval : config -> prg -> config

   Takes a configuration and a program, and returns a configuration as a result
 *)                         
let rec eval (stack, (state, inp, out)) prg = match prg with
    | []               -> (stack, (state, inp, out))
    | instruction::prg' -> match instruction with
        | BINOP op -> eval (let y::x::stack' = stack in ((Expr.evalBinop op x y)::stack', (state, inp, out))) prg'
        | CONST v  -> eval (v::stack, (state, inp, out)) prg'
        | READ     -> eval (let z::inp' = inp in (z::stack, (state, inp', out))) prg'
        | WRITE    -> eval (let z::stack' = stack in (stack', (state, inp, out @ [z]))) prg'
        | LD symb  -> eval ((state symb)::stack, (state, inp, out)) prg'
        | ST symb  -> eval (let z::stack' = stack in (stack', (Expr.update symb z state, inp, out))) prg'
        | _        -> failwith "SM Interpreter: Runtime error."

(* Top-level evaluation
     val run : int list -> prg -> int list
   Takes an input stream, a program, and returns an output stream this program calculates
*)
let run i p = let (_, (_, _, o)) = eval ([], (Expr.empty, i, [])) p in o


(* Stack machine compiler
     val compile : Expr.t -> prg
   Takes a program in the source language and returns an equivalent program for the
   stack machine
 *)
let rec compileExpr e = match e with
    | Expr.Const c          -> [CONST c] 
    | Expr.Var v            -> [LD v]
    | Expr.Binop (op, l, r) -> compileExpr l @ compileExpr r @ [BINOP op]

(* Stack machine compiler
     val compile : Stmt.t -> prg
   Takes a program in the source language and returns an equivalent program for the
   stack machine
 *)
let rec compile stmt = match stmt with
    | Stmt.Read symb          -> [READ; ST symb]
    | Stmt.Write e            -> compileExpr e @ [WRITE]
    | Stmt.Assign (symb, e)   -> compileExpr e @ [ST symb]
    | Stmt.Seq (stmt1, stmt2) -> compile stmt1 @ compile stmt2

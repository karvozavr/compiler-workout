open GT       
open Language
       
(* The type for the stack machine instructions *)
@type insn =
(* binary operator                 *) | BINOP of string
(* put a constant on the stack     *) | CONST of int                 
(* read to stack                   *) | READ
(* write from stack                *) | WRITE
(* load a variable to the stack    *) | LD    of string
(* store a variable from the stack *) | ST    of string
(* a label                         *) | LABEL of string
(* unconditional jump              *) | JMP   of string                                                                                                                
(* conditional jump                *) | CJMP  of string * string with show
                                                   
(* The type for the stack machine program *)                                                               
type prg = insn list

(* The type for the stack machine configuration: a stack and a configuration from statement
   interpreter
 *)
type config = int list * Stmt.config

(* Stack machine interpreter

     val eval : env -> config -> prg -> config

   Takes an environment, a configuration and a program, and returns a configuration as a result. The
   environment is used to locate a label to jump to (via method env#labeled <label_name>)
*)                         
let rec eval env conf prg = 
  let (stack, (state, inp, out)) = conf in match prg with
    | []               -> conf
    | instruction::prg' -> match instruction with
        | BINOP op    -> eval env (let y::x::stack' = stack in ((Expr.evalBinop op x y)::stack', (state, inp, out))) prg'
        | CONST v     -> eval env (v::stack, (state, inp, out)) prg'
        | READ        -> eval env (let z::inp' = inp in (z::stack, (state, inp', out))) prg'
        | WRITE       -> eval env (let z::stack' = stack in (stack', (state, inp, out @ [z]))) prg'
        | LD symb     -> eval env ((state symb)::stack, (state, inp, out)) prg'
        | ST symb     -> eval env (let z::stack' = stack in (stack', (Expr.update symb z state, inp, out))) prg'
        | LABEL label -> eval env conf prg'
        | JMP label   -> eval env conf (env#labeled label)
        | CJMP (op, label) -> 
          let cond::stack' = stack in 
          let check = match op with 
            | "z"  -> (==) 0
            | "nz" -> (<>) 0
          in eval env conf (if (check cond) then (env#labeled label) else prg') 
        | _           -> failwith "SM Interpreter: Runtime error."


(* Top-level evaluation

     val run : prg -> int list -> int list

   Takes a program, an input stream, and returns an output stream this program calculates
*)
let run p i =
  let module M = Map.Make (String) in
  let rec make_map m = function
  | []              -> m
  | (LABEL l) :: tl -> make_map (M.add l tl m) tl
  | _ :: tl         -> make_map m tl
  in
  let m = make_map M.empty p in
  let (_, (_, _, o)) = eval (object method labeled l = M.find l m end) ([], (Expr.empty, i, [])) p in o

class labelGenerator =
  object (self)
    val mutable label = 0

    method new_label = 
      label <- label + 1;
      Printf.sprintf "L%d" label
  end

(* Stack machine compiler

     val compile : Language.Stmt.t -> prg

   Takes a program in the source language and returns an equivalent program for the
   stack machine
*)
let rec compile stmt = 
  let rec compile' labelGenerator = 
    let rec expr = function
    | Expr.Var   x          -> [LD x]
    | Expr.Const n          -> [CONST n]
    | Expr.Binop (op, x, y) -> expr x @ expr y @ [BINOP op]
    in
    function
    | Stmt.Seq (s1, s2)       -> compile' labelGenerator s1 @ compile' labelGenerator s2
    | Stmt.Read x             -> [READ; ST x]
    | Stmt.Write e            -> expr e @ [WRITE]
    | Stmt.Assign (x, e)      -> expr e @ [ST x]
    | Stmt.Skip               -> []
    | Stmt.If (cond, tr, fls) -> 
      let else_label  = labelGenerator#new_label in
      let end_label = labelGenerator#new_label 
      in expr cond 
        @ [CJMP("z", else_label)]
        @ compile' labelGenerator tr
        @ [JMP end_label; LABEL else_label]
        @ compile' labelGenerator fls
        @ [LABEL end_label]
    | Stmt.While (cond, body) -> 
      let start_label = labelGenerator#new_label in
      let end_label  = labelGenerator#new_label
      in [LABEL start_label]
        @ expr cond
        @ [CJMP("z", end_label)]
        @ compile' labelGenerator body
        @ [JMP start_label; LABEL end_label]
    | Stmt.Repeat (cond, body) -> 
      let start_label = labelGenerator#new_label 
      in [LABEL start_label]
        @ compile' labelGenerator body
        @ expr cond
        @ [CJMP("z", start_label)]

  in compile' (new labelGenerator) stmt

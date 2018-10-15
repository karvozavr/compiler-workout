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
(* conditional jump                *) | CJMP  of string * string
(* begins procedure definition     *) | BEGIN of string list * string list
(* end procedure definition        *) | END
(* calls a procedure               *) | CALL  of string with show
                                                   
(* The type for the stack machine program *)                                                               
type prg = insn list

(* The type for the stack machine configuration: control stack, stack and configuration from statement
   interpreter
 *)
type config = (prg * State.t) list * int list * Stmt.config

(* Stack machine interpreter

     val eval : env -> config -> prg -> config

   Takes an environment, a configuration and a program, and returns a configuration as a result. The
   environment is used to locate a label to jump to (via method env#labeled <label_name>)
*)                         
let rec eval env conf prg = 
  let (call_stack, stack, (state, inp, out)) = conf in match prg with
    | []               -> conf
    | instruction::prg' -> match instruction with
        | BINOP op    -> eval env (let y::x::stack' = stack in (call_stack, (Expr.eval_binop op x y)::stack', (state, inp, out))) prg'
        | CONST v     -> eval env (call_stack, v::stack, (state, inp, out)) prg'
        | READ        -> eval env (let z::inp' = inp in (call_stack, z::stack, (state, inp', out))) prg'
        | WRITE       -> eval env (let z::stack' = stack in (call_stack, stack', (state, inp, out @ [z]))) prg'
        | LD symb     -> eval env (call_stack, (State.eval state symb)::stack, (state, inp, out)) prg'
        | ST symb     -> eval env (let z::stack' = stack in (call_stack, stack', (State.update symb z state, inp, out))) prg'
        | LABEL label -> eval env conf prg'
        | JMP label   -> eval env conf (env#labeled label)
        | CJMP (op, label) -> 
          let cond::stack' = stack in 
          let check = match op with 
            | "z"  -> (==) 0
            | "nz" -> (<>) 0
          in eval env conf (if (check cond) then (env#labeled label) else prg') 
        | CALL name   -> eval env ((prg', state)::call_stack, stack, (state, inp, out)) (env#labeled name)
        | BEGIN (params, locals) -> 
          let inner_state = State.push_scope state (params @ locals) in 
          let (state', stack') = List.fold_right 
            (fun symb (st, z::stack') -> (State.update symb z st, stack')) params (inner_state, stack)
          in eval env (call_stack, stack, (state', inp, out)) prg'
        | END         -> 
          let (prog, state')::call_stack' = call_stack in 
          eval env (call_stack', stack, (State.drop_scope state state', inp, out)) prog
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
  let (_, _, (_, _, o)) = eval (object method labeled l = M.find l m end) ([], [], (State.empty, i, [])) p in o

class labelGenerator =
  object (self)
    val mutable label = 0

    method new_label = 
      label <- label + 1;
      Printf.sprintf "L%d" label
  end

(* Stack machine compiler

     val compile : Language.t -> prg

   Takes a program in the source language and returns an equivalent program for the
   stack machine
*)
let rec compile (defs, stmt) = 
  let rec compile_stmt labelGenerator = 
    let rec expr = function
    | Expr.Var   x          -> [LD x]
    | Expr.Const n          -> [CONST n]
    | Expr.Binop (op, x, y) -> expr x @ expr y @ [BINOP op]
    in
    function
    | Stmt.Seq (s1, s2)       -> compile_stmt labelGenerator s1 @ compile_stmt labelGenerator s2
    | Stmt.Read x             -> [READ; ST x]
    | Stmt.Write e            -> expr e @ [WRITE]
    | Stmt.Assign (x, e)      -> expr e @ [ST x]
    | Stmt.Skip               -> []
    | Stmt.If (cond, tr, fls) -> 
      let else_label  = labelGenerator#new_label in
      let end_label = labelGenerator#new_label 
      in expr cond 
        @ [CJMP("z", else_label)]
        @ compile_stmt labelGenerator tr
        @ [JMP end_label; LABEL else_label]
        @ compile_stmt labelGenerator fls
        @ [LABEL end_label]
    | Stmt.While (cond, body) -> 
      let start_label = labelGenerator#new_label in
      let end_label  = labelGenerator#new_label
      in [LABEL start_label]
        @ expr cond
        @ [CJMP("z", end_label)]
        @ compile_stmt labelGenerator body
        @ [JMP start_label; LABEL end_label]
    | Stmt.Repeat (body, cond) -> 
      let start_label = labelGenerator#new_label 
      in [LABEL start_label]
        @ compile_stmt labelGenerator body
        @ expr cond
        @ [CJMP("z", start_label)]
    | Stmt.Call (name, args) -> List.concat (List.map expr args) @ [CALL name]
  in 
  let compile_procedure labelGenerator (func_name, (args, local_vars, func_body)) = 
    [LABEL func_name; BEGIN (args, local_vars)]
    @ compile_stmt labelGenerator func_body
    @ [END] 
  in 
  let label_generator = new labelGenerator in
  let main_label = label_generator#new_label in 
  [JMP main_label]
  @ List.concat (List.map (compile_procedure label_generator) defs)
  @ [LABEL main_label]
  @ compile_stmt label_generator stmt

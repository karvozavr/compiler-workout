(* Opening a library for generic programming (https://github.com/dboulytchev/GT).
   The library provides "@type ..." syntax extension and plugins like show, etc.
*)
open GT

(* Opening a library for combinator-based syntax analysis *)
open Ostap.Combinators
open Ostap
open List

(* States *)
module State =
  struct
                                                                
    (* State: global state, local state, scope variables *)
    type t = {g : string -> int; l : string -> int; scope : string list}

    (* Empty state *)
    let empty =
      let e x = failwith (Printf.sprintf "Undefined variable: %s" x) in
      {g = e; l = e; scope = []}

    (* Update: non-destructively "modifies" the state s by binding the variable x 
       to value v and returns the new state w.r.t. a scope
    *)
    let update x v s =
      let u x v s = fun y -> if x = y then v else s y in
      if List.mem x s.scope then {s with l = u x v s.l} else {s with g = u x v s.g}

    (* Evals a variable in a state w.r.t. a scope *)
    let eval s x = (if List.mem x s.scope then s.l else s.g) x

    (* Creates a new scope, based on a given state *)
    let push_scope st xs = {empty with g = st.g; scope = xs}

    (* Drops a scope *)
    let drop_scope st st' = {st' with g = st.g}

  end
    
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
      
    (* Expression evaluator

          val eval : state -> t -> int
 
       Takes a state and an expression, and returns the value of the expression in 
       the given state.
    *)                                                       
    let to_func op =
      let bti   = function true -> 1 | _ -> 0 in
      let itb b = b <> 0 in
      let (|>) f g   = fun x y -> f (g x y) in
      match op with
      | "+"  -> (+)
      | "-"  -> (-)
      | "*"  -> ( * )
      | "/"  -> (/)
      | "%"  -> (mod)
      | "<"  -> bti |> (< )
      | "<=" -> bti |> (<=)
      | ">"  -> bti |> (> )
      | ">=" -> bti |> (>=)
      | "==" -> bti |> (= )
      | "!=" -> bti |> (<>)
      | "&&" -> fun x y -> bti (itb x && itb y)
      | "!!" -> fun x y -> bti (itb x || itb y)
      | _    -> failwith (Printf.sprintf "Unknown binary operator %s" op)    
    
    let evalBinop op x y = (to_func op) x y

    let rec eval st expr =      
      match expr with
      | Const n -> n
      | Var   x -> State.eval st x
      | Binop (op, x, y) -> to_func op (eval st x) (eval st y)

    (* Expression parser. You can use the following terminals:

         IDENT   --- a non-empty identifier a-zA-Z[a-zA-Z0-9_]* as a string
         DECIMAL --- a decimal constant [0-9]+ as a string
                                                                                                                  
    *)
    ostap (                                      
      parse:
    !(Ostap.Util.expr 
      (fun x -> x)
      (Array.map (fun (a, s) -> a, 
                         List.map  (fun s -> ostap(- $(s)), (fun x y -> Binop (s, x, y))) s
                      ) 
            [|                
              `Lefta, ["!!"];
              `Lefta, ["&&"];
              `Nona , ["=="; "!="; "<="; "<"; ">="; ">"];
              `Lefta, ["+" ; "-"];
              `Lefta, ["*" ; "/"; "%"];
            |] 
      )
      primary);
      
      primary:
        n:DECIMAL {Const n}
      | x:IDENT   {Var x}
      | -"(" parse -")"
    )
    
  end

let default x opt = match opt with
        | Some v -> v
        | None   -> x
             
(* Simple statements: syntax and sematics *)
module Stmt =
  struct

    (* The type for statements *)
    @type t =
    (* read into the variable           *) | Read   of string
    (* write the value of an expression *) | Write  of Expr.t
    (* assignment                       *) | Assign of string * Expr.t
    (* composition                      *) | Seq    of t * t 
    (* empty statement                  *) | Skip
    (* conditional                      *) | If     of Expr.t * t * t
    (* loop with a pre-condition        *) | While  of Expr.t * t
    (* loop with a post-condition       *) | Repeat of t * Expr.t
    (* call a procedure                 *) | Call   of string * Expr.t list with show
                                                                    
    (* The type of configuration: a state, an input stream, an output stream *)
    type config = State.t * int list * int list 

    (* Statement evaluator

         val eval : env -> config -> t -> config

       Takes an environment, a configuration and a statement, and returns another configuration. The 
       environment supplies the following method

           method definition : string -> (string list, string list, t)

       which returns a list of formal parameters and a body for given definition
    *)
     let rec eval env (state, inp, out) stmt = 
      let conf = (state, inp, out) in match stmt with
        | Read symb          -> (State.update symb (hd inp) state, tl inp, out)
        | Write e            -> (state, inp, out @ [Expr.eval state e]) 
        | Assign (symb, e)   -> (State.update symb (Expr.eval state e) state, inp, out)
        | Seq (stmt1, stmt2) -> eval env (eval env (state, inp, out) stmt1) stmt2
        | Skip               -> conf
        | If (cond, tr, fls) -> if (Expr.eval state cond) <> 0 
                                  then eval env conf tr 
                                  else eval env conf fls
        | While (cond, body) -> if (Expr.eval state cond) <> 0 
                                  then eval env (eval env conf body) (While (cond, body)) 
                                else conf
        | Repeat (body, cond) -> 
          let (state', inp', out') = eval env conf body in
          if (Expr.eval state' cond) == 0
            then eval env (state', inp', out') (Repeat (body, cond))
            else (state', inp', out') 
        | Call (name, args) -> 
          let arg_vals = List.map (Expr.eval state) args in
          let (args, local_vars, func_body) = env#definition name in
          let inner_state = State.push_scope state (args @ local_vars) in 
          let inner_state = List.fold_left2 (fun st arg value -> State.update arg value st) inner_state args arg_vals in
          let (state', inp', out') = eval env (inner_state, inp, out) func_body in 
          (State.drop_scope state' state, inp', out')  

    (* Statement parser *)
    ostap (
      assign: 
        x:IDENT -":=" e:!(Expr.parse) 
        {Assign (x, e)};
      read: 
        -"read" -"(" x:IDENT -")" 
        {Read (x)};
      write:
        -"write" -"(" e:!(Expr.parse) -")" 
        {Write (e)};
      whileLoop:
        -"while" cond:!(Expr.parse) -"do" body:!(parse) -"od" 
        {While (cond, body)};
      ifStmt:
          -"if" cond:!(Expr.parse) -"then" tr:!(parse) fls:!(elseStmt) -"fi" 
          {If (cond, tr, fls)}
        | -"if" cond:!(Expr.parse) -"then" tr:!(parse) -"fi" 
          {If (cond, tr, Skip)};
      elseStmt:
          -"else" fls:!(parse)
          {fls}
        | -"elif" cond:!(Expr.parse) -"then" tr:!(parse) fls:!(elseStmt)
          {If (cond, tr, fls)};
      skip:
        -"skip"
        {Skip};
      forLoop:
        -"for" ini:!(parse) -"," cond:!(Expr.parse) "," post:!(parse) -"do" body:!(parse) -"od" 
        {Seq (ini, While (cond, Seq (body, post)))};
      repeatLoop:
        -"repeat" body:!(parse) -"until" cond:!(Expr.parse) 
        {Repeat (body, cond)};
      functionCall:
        funcName:IDENT -"(" args:(!(Util.list)[ostap (!(Expr.parse))])? -")"
        {Call (funcName, default [] args)}; 

      stmt: read | write | assign | skip | ifStmt | whileLoop | forLoop | repeatLoop | functionCall;
      parse: 
        <x::xs> 
          :!(Util.listBy)
          [ostap (";")]
          [stmt]
          {List.fold_left (fun x y -> Seq (x, y)) x xs}
    )
      
  end

(* Function and procedure definitions *)
module Definition =
  struct

    (* The type for a definition: name, argument list, local variables, body *)
    type t = string * (string list * string list * Stmt.t)

    ostap (                                      
      parse: 
        -"fun" funcName:IDENT -"(" args:(!(Util.list)[ostap (IDENT)])? -")" localVars:(-"local" !(Util.list)[ostap (IDENT)])? -"{" body:!(Stmt.parse) -"}"
        {(funcName, (default [] args, default [] localVars, body))}
    )

  end
    
(* The top-level definitions *)

(* The top-level syntax category is a pair of definition list and statement (program body) *)
type t = Definition.t list * Stmt.t    

(* Top-level evaluator

     eval : t -> int list -> int list

   Takes a program and its input stream, and returns the output stream
*)
let eval (defs, body) i =
  let module M = Map.Make (String) in
  let m        = List.fold_left (fun m ((name, _) as def) -> M.add name def m) M.empty defs in  
  let _, _, o  = Stmt.eval (object method definition f = snd @@ M.find f m end) (State.empty, i, []) body in o

(* Top-level parser *)
let parse = ostap (!(Definition.parse)* !(Stmt.parse))

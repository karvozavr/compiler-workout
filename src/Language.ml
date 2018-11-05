(* Opening a library for generic programming (https://github.com/dboulytchev/GT).
   The library provides "@type ..." syntax extension and plugins like show, etc.
*)
open GT
open List

(* Opening a library for combinator-based syntax analysis *)
open Ostap.Combinators
open Ostap

                         
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
    let enter st xs = {empty with g = st.g; scope = xs}

    (* Drops a scope *)
    let leave st st' = {st' with g = st.g}

  end
    
let default x opt = match opt with
  | Some v -> v
  | None   -> x

(* Simple expressions: syntax and semantics *)
module Expr =
  struct
    
    (* The type for expressions. Note, in regular OCaml there is no "@type..." 
       notation, it came from GT. 
    *)
    @type t =
    (* integer constant *) | Const of int
    (* variable         *) | Var   of string
    (* binary operator  *) | Binop of string * t * t
    (* function call    *) | Call  of string * t list with show

    (* Available binary operators:
        !!                   --- disjunction
        &&                   --- conjunction
        ==, !=, <=, <, >=, > --- comparisons
        +, -                 --- addition, subtraction
        *, /, %              --- multiplication, division, reminder
    *)

    (* The type of configuration: a state, an input stream, an output stream, an optional value *)
    type config = State.t * int list * int list * int option
          
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
    
    let eval_binop op x y = (to_func op) x y

    (* Expression evaluator

          val eval : env -> config -> t -> int * config


       Takes an environment, a configuration and an expresion, and returns another configuration. The 
       environment supplies the following method

           method definition : env -> string -> int list -> config -> config

       which takes an environment (of the same type), a name of the function, a list of actual parameters and a configuration, 
       an returns a pair: the return value for the call and the resulting configuration
    *)                                                       
    let rec eval env ((st, i, o, r) as conf) expr = match expr with      
      | Const n -> n, (st, i, o, Some n)
      | Var   x -> let n = State.eval st x in n, (st, i, o, Some n)
      | Binop (op, x, y) ->
        let lhs, conf = eval env conf x in 
        let rhs, conf = eval env conf y in 
        let n = to_func op lhs rhs in 
        n, (st, i, o, Some n)
      | Call (func_name, exprs) -> 
        let expr_to_args expr (args, conf') = let arg, conf' = eval env conf' expr in (arg::args, conf') in
        let args, conf = List.fold_right expr_to_args exprs ([], conf) in 
        let st', i', o', Some r' = env#definition env func_name args conf in 
        r', (State.leave st' st, i', o', Some r')

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

      func: 
        funcName:IDENT -"(" args:(!(Util.list)[ostap (!(parse))])? -")"
        {Call (funcName, default [] args)}; 

      primary:
        func
      | n:DECIMAL {Const n}
      | x:IDENT   {Var x}
      | -"(" parse -")"
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
    (* composition                      *) | Seq    of t * t 
    (* empty statement                  *) | Skip
    (* conditional                      *) | If     of Expr.t * t * t
    (* loop with a pre-condition        *) | While  of Expr.t * t
    (* loop with a post-condition       *) | Repeat of t * Expr.t
    (* return statement                 *) | Return of Expr.t option
    (* call a procedure                 *) | Call   of string * Expr.t list with show

    (*
      Diamond meta-operator.

      val (<>) : Stmt -> Stmt -> Stmt
    *)
    let (<>) s1 s2 = match s2 with
      | Skip -> s1
      | _    -> Seq (s1, s2)

    (* Statement evaluator

         val eval : env -> config -> t -> config

       Takes an environment, a configuration and a statement, and returns another configuration. The 
       environment is the same as for expressions
    *)
    let rec eval env ((state, inp, out, r) as conf) k stmt = match stmt with
      | Skip      -> if k == Skip then conf else (eval env conf Skip k)      
      | Read symb -> eval env (State.update symb (hd inp) state, tl inp, out, r) Skip k
      | Write e   -> 
        let v, (s', i', o', r') = Expr.eval env conf e in  
        eval env (s', i', o' @ [v], r') Skip k 
      | Assign (symb, e) -> 
        let v, ((s', i', o', r') as conf') = Expr.eval env conf e in
        eval env (State.update symb v s', i', o', r') Skip k
      | Seq (stmt1, stmt2) -> eval env conf (stmt2 <> k) stmt1
      | If (cond, tr, fls) -> 
        let v, conf' = Expr.eval env conf cond in
        let s' = if v != 0 then tr else fls in
        eval env conf' k s' 
      | While (cond, body) -> 
        let v, conf' = Expr.eval env conf cond in
        let k', body' = if v != 0 then While (cond, body) <> k, body else Skip, k in
        eval env conf' k' body' 
      | Repeat (body, cond) -> 
        let cond' = Expr.Binop ("==", cond, Const 0) in 
        eval env conf (While (cond', body) <> k) body
      | Call (name, exprs) -> 
        let _, conf' = Expr.eval env conf (Expr.Call (name, exprs)) in 
        eval env conf' Skip k
      | Return r -> match r with
        | None   -> conf
        | Some e -> snd (Expr.eval env conf e) 

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
      returnStmt:
        -"return" e:!(Expr.parse)? 
        {Return e};

      stmt: returnStmt | read | write | assign | skip | ifStmt | whileLoop | forLoop | repeatLoop | functionCall;
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
      arg  : IDENT;
      parse: %"fun" name:IDENT "(" args:!(Util.list0 arg) ")"
         locs:(%"local" !(Util.list arg))?
        "{" body:!(Stmt.parse) "}" {
        (name, (args, (match locs with None -> [] | Some l -> l), body))
      }
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
  let m          = List.fold_left (fun m ((name, _) as def) -> M.add name def m) M.empty defs in  
  let _, _, o, _ =
    Stmt.eval
      (object
         method definition env f args (st, i, o, r) =
           let xs, locs, s      = snd @@ M.find f m in
           let st'              = List.fold_left (fun st (x, a) -> State.update x a st) (State.enter st (xs @ locs)) (List.combine xs args) in
           let st'', i', o', r' = Stmt.eval env (st', i, o, r) Stmt.Skip s in
           (State.leave st'' st, i', o', r')
       end)
      (State.empty, i, [], None)
      Stmt.Skip
      body
  in
  o

(* Top-level parser *)
let parse = ostap (!(Definition.parse)* !(Stmt.parse))

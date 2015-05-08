open Ast;;
open Env;;
open Store;;

let rec allocateSymbol (table:symTable):symTable =
match table with |
[]->[] |
(strn,Var vEntry)::xz -> (strn, Var{vEntry with loc = vEntry.loc+1})::allocateSymbol(xz);;

let rec allocateMem (env:environment) : environment = 
match env with | 
[]->[] |
(symTable::xz)->allocateSymbol(symTable) :: allocateMem(xz);; 

(* eval_expr and eval_cond don't return a store, but you might have to modify
that because in full C-flat they do have side effets (pre- and post-increment,
function calls) *)

(* eval_expr: expr -> proc_state -> env -> store -> int *)
let rec eval_expr (expr:expr) (ps:proc_state) (env:environment) (store:store) : int = match expr with
    Add (e1, e2) -> 
      let r1 = eval_expr e1 ps env store in
      let r2 = eval_expr e2 ps env store in
      r1 + r2
  | Sub (e1, e2) ->
      let r1 = eval_expr e1 ps env store in
      let r2 = eval_expr e2 ps env store in
      r1 - r2
  | Mul (e1, e2) ->
      let r1 = eval_expr e1 ps env store in
      let r2 = eval_expr e2 ps env store in
      r1 * r2
  | Div (e1, e2) ->
      let r1 = eval_expr e1 ps env store in
      let r2 = eval_expr e2 ps env store in
      r1 / r2
  | Neg (e1) ->
      let r1 = eval_expr e1 ps env store in
      -r1
  | IntConst i -> i
;;

(* eval_expr: expr -> proc_state -> env -> store -> bool *)
let rec eval_cond (cond:cond) (ps:proc_state) (env:environment) (store:store) : bool = match cond with
  Equal (e1, e2) ->
    let r1 = eval_expr e1 ps env store in
    let r2 = eval_expr e2 ps env store in
    r1 == r2
(* TODO: add more *)
;;

type stmtEvalRes = Next | BreakOut | ContinueOn;;

(* eval_stmt: stmt -> proc_state -> env -> store -> stmtEvalRes*proc_state*store *)
let rec eval_stmt (stmt:stmt) (ps:proc_state) (env:environment) (store:store) : (stmtEvalRes*proc_state*store) = match stmt with
| PrintInt e ->
    let r = eval_expr e ps env store in
    print_int r; (Next, ps, store)
| PrintStr s ->
    print_string (Str.global_replace (Str.regexp "\\\\n") "\n" s); 
    (* Escaping characters here because it's not done in the parser *)
    (Next, ps, store)
| List (stmt1::stmts) -> eval_stmt stmt1 ps env store; (Next, ps, store)

and eval_stmt_list(stmts:stmt list) (ps:proc_state) (env:environment) (store:store) : (stmtEvalRes * proc_state * store) = match stmts with
  | x::[] -> eval_stmt x ps env store;;
  | x::xs -> eval_stmt x ps env store; eval_stmt_list xs ps env store

  (* TODO: complete this case so that all statements in the list evaluated *)
;;

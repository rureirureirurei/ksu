(* Name sanitizer for C code generation *)
open Lang
open Ast

(* Replace dangerous characters in variable names with C-safe alternatives *)
let sanitize_var_name (name: string) : string =
  let char_map = function
    | '+' -> "_plus_"
    | '-' -> "_minus_"
    | '?' -> "_quest_"
    | '!' -> "_bang_"
    | '=' -> "_equal_"
    | '<' -> "_lt_"
    | '>' -> "_gt_"
    | '*' -> "_star_"
    | '/' -> "_slash_"
    | '%' -> "_percent_"
    | '&' -> "_amp_"
    | '|' -> "_pipe_"
    | '^' -> "_caret_"
    | '~' -> "_tilde_"
    | c -> String.make 1 c
  in
  let buf = Buffer.create (String.length name * 2) in
  String.iter (fun c -> Buffer.add_string buf (char_map c)) name;
  Buffer.contents buf

(* Sanitize a list of variable names *)
let sanitize_var_list (vars: var list) : var list =
  List.map sanitize_var_name vars

(* Sanitize let bindings *)
let rec sanitize_let_bindings (bindings: (var * expr) list) : (var * expr) list =
  List.map (fun (var, expr) -> (sanitize_var_name var, sanitize_expr expr)) bindings

(* Sanitize lambda arguments *)
and sanitize_lambda_args (args: var list) : var list =
  List.map sanitize_var_name args

(* Sanitize define expressions *)
and sanitize_define (name: var) : var =
  sanitize_var_name name

(* Sanitize an entire expression recursively *)
and sanitize_expr (expr: expr) : expr =
  match expr.value with
  | E_Var v -> { expr with value = E_Var (sanitize_var_name v) }
  | E_App (func, args) -> 
      { expr with value = E_App (sanitize_expr func, List.map sanitize_expr args) }
  | E_Lambda (args, body) ->
      { expr with value = E_Lambda (sanitize_lambda_args args, sanitize_expr body) }
  | E_If (cond, then_expr, else_expr) ->
      { expr with value = E_If (sanitize_expr cond, sanitize_expr then_expr, sanitize_expr else_expr) }
  | E_Callcc e -> { expr with value = E_Callcc (sanitize_expr e) }
  | E_Begin exprs -> { expr with value = E_Begin (List.map sanitize_expr exprs) }
  | E_Let (bindings, body) ->
      { expr with value = E_Let (sanitize_let_bindings bindings, sanitize_expr body) }
  | E_Pair (e1, e2) -> { expr with value = E_Pair (sanitize_expr e1, sanitize_expr e2) }
  | E_Bool _
  | E_Number _ 
  | E_String _
  | E_Prim _
  | E_Nil -> expr

(* Sanitize a top-level expression *)
let sanitize_top_expr (top_expr: top_expr) : top_expr =
  match top_expr.value with
  | E_Expr e -> { top_expr with value = E_Expr (sanitize_expr e) }
  | E_Define (name, expr) -> 
      { top_expr with value = E_Define (sanitize_define name, sanitize_expr expr) }

(* Sanitize a list of top-level expressions *)
let sanitize_top_exprs (top_exprs: top_expr list) : top_expr list =
  List.map sanitize_top_expr top_exprs

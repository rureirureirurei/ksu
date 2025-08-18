(* Name sanitizer for C code generation *)
open Compiler_lib
open Ast

(* Replace dangerous characters in variable names with C-safe alternatives *)
let sanitize_var_name (name: string) : string =
  let rec sanitize_chars chars =
    match chars with
    | [] -> []
    | '+' :: rest -> '_' :: 'p' :: 'l' :: 'u' :: 's' :: '_' :: sanitize_chars rest
    | '-' :: rest -> '_' :: 'm' :: 'i' :: 'n' :: 'u' :: 's' :: '_' :: sanitize_chars rest
    | '?' :: rest -> '_' :: 'q' :: 'u' :: 'e' :: 's' :: 't' :: '_' :: sanitize_chars rest
    | '!' :: rest -> '_' :: 'b' :: 'a' :: 'n' :: 'g' :: '_' :: sanitize_chars rest
    | '=' :: rest -> '_' :: 'e' :: 'q' :: 'u' :: 'a' :: 'l' :: '_' :: sanitize_chars rest
    | '<' :: rest -> '_' :: 'l' :: 't' :: '_' :: sanitize_chars rest
    | '>' :: rest -> '_' :: 'g' :: 't' :: '_' :: sanitize_chars rest
    | '*' :: rest -> '_' :: 's' :: 't' :: 'a' :: 'r' :: '_' :: sanitize_chars rest
    | '/' :: rest -> '_' :: 's' :: 'l' :: 'a' :: 's' :: 'h' :: '_' :: sanitize_chars rest
    | '%' :: rest -> '_' :: 'p' :: 'e' :: 'r' :: 'c' :: 'e' :: 'n' :: 't' :: '_' :: sanitize_chars rest
    | '&' :: rest -> '_' :: 'a' :: 'm' :: 'p' :: '_' :: sanitize_chars rest
    | '|' :: rest -> '_' :: 'p' :: 'i' :: 'p' :: 'e' :: '_' :: sanitize_chars rest
    | '^' :: rest -> '_' :: 'c' :: 'a' :: 'r' :: 'e' :: 't' :: '_' :: sanitize_chars rest
    | '~' :: rest -> '_' :: 't' :: 'i' :: 'l' :: 'd' :: 'e' :: '_' :: sanitize_chars rest
    | c :: rest -> c :: sanitize_chars rest
  in
  String.of_seq (List.to_seq (sanitize_chars (String.to_seq name |> List.of_seq)))

(* Sanitize a list of variable names *)
let sanitize_var_list (vars: var list) : var list =
  List.map sanitize_var_name vars

(* Sanitize let bindings *)
let sanitize_let_bindings (bindings: (var * expr) list) : (var * expr) list =
  List.map (fun (var, expr) -> (sanitize_var_name var, expr)) bindings

(* Sanitize lambda arguments *)
let sanitize_lambda_args (args: var list) : var list =
  List.map sanitize_var_name args

(* Sanitize define expressions *)
let sanitize_define (name: var) : var =
  sanitize_var_name name

(* Sanitize an entire expression recursively *)
let rec sanitize_expr (expr: expr) : expr =
  match expr.value with
  | E_Var v -> { expr with value = E_Var (sanitize_var_name v) }
  | E_App (func, args) -> 
      { expr with value = E_App (sanitize_expr func, List.map sanitize_expr args) }
  | E_Lambda (args, body) ->
      { expr with value = E_Lambda (sanitize_lambda_args args, sanitize_expr body) }
  | E_If (cond, then_expr, else_expr) ->
      { expr with value = E_If (sanitize_expr cond, sanitize_expr then_expr, sanitize_expr else_expr) }
  | E_Callcc e -> { expr with value = E_Callcc (sanitize_expr e) }
  | E_Let (bindings, body) ->
      { expr with value = E_Let (sanitize_let_bindings bindings, sanitize_expr body) }
  | E_Pair (e1, e2) -> { expr with value = E_Pair (sanitize_expr e1, sanitize_expr e2) }
  | E_Bool _
  | E_Number _ 
  | E_String _
  | E_Nil 
  | E_PrimApp _ -> expr

(* Sanitize a top-level expression *)
let sanitize_top_expr (top_expr: top_expr) : top_expr =
  match top_expr.value with
  | E_Expr e -> { top_expr with value = E_Expr (sanitize_expr e) }
  | E_Define (name, expr) -> 
      { top_expr with value = E_Define (sanitize_define name, sanitize_expr expr) }

(* Sanitize a list of top-level expressions *)
let sanitize_top_exprs (top_exprs: top_expr list) : top_expr list =
  List.map sanitize_top_expr top_exprs

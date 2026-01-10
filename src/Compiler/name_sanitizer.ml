(* Name sanitizer for C code generation *)
open Lang
open Ast

(* C reserved words that need prefixing *)
let c_reserved = ["and"; "or"; "not"; "int"; "char"; "void"; "if"; "else";
                  "while"; "for"; "return"; "break"; "continue"; "switch";
                  "case"; "default"; "struct"; "union"; "enum"; "typedef";
                  "static"; "extern"; "const"; "volatile"; "auto"; "register";
                  "sizeof"; "goto"; "do"; "float"; "double"; "long"; "short";
                  "signed"; "unsigned"; "true"; "false"; "bool"; "NULL"]

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
  let result = Buffer.contents buf in
  (* Prefix reserved words with underscore *)
  if List.mem result c_reserved then "_" ^ result else result

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
  match expr with
  | E_Var v -> E_Var (sanitize_var_name v)
  | E_App (func, args) ->
      E_App (sanitize_expr func, List.map sanitize_expr args)
  | E_Lambda (args, body) ->
      E_Lambda (sanitize_lambda_args args, sanitize_expr body)
  | E_If (cond, then_expr, else_expr) ->
      E_If (sanitize_expr cond, sanitize_expr then_expr, sanitize_expr else_expr)
  | E_Callcc (v, e) -> E_Callcc (sanitize_var_name v, sanitize_expr e)
  | E_Begin exprs -> E_Begin (List.map sanitize_expr exprs)
  | E_Let (bindings, body) ->
      E_Let (sanitize_let_bindings bindings, sanitize_expr body)
  | E_Bool _
  | E_Number _
  | E_String _
  | E_Prim _ -> expr

(* Sanitize a top-level expression *)
let sanitize_top_expr (top_expr: top_expr) : top_expr =
  match top_expr with
  | E_Expr e -> E_Expr (sanitize_expr e)
  | E_Define (name, expr) ->
      E_Define (sanitize_define name, sanitize_expr expr)

(* Sanitize a list of top-level expressions *)
let sanitize_top_exprs (top_exprs: top_expr list) : top_expr list =
  List.map sanitize_top_expr top_exprs

open Compiler_lib
open Ast

(* Helper function to create synthetic expressions *)
let syn expr = { value = expr; loc = { file = ""; line = 0; column = 0 } }

(* Helper function to create lambda with given parameters *)
let mk_lambda params body = syn (E_Lambda (params, body))

(* Helper function to create primitive application *)
let mk_prim_app prim vars = syn (E_PrimApp (prim, vars))

(* Helper function to create variable reference *)
let mk_var name = syn (E_Var name)

(* Helper function to create function application *)
let mk_app func args = syn (E_App (func, args))

(* Helper function to create define *)
let mk_define name expr = syn (E_Define (name, expr))

(* Builtin primitive definitions as AST expressions *)
let builtin_definitions : top_expr list =
  [
    (* Arithmetic primitives *)
    mk_define "+" (mk_lambda ["a0"; "a1"] (mk_prim_app P_Plus ["a0"; "a1"]));
    mk_define "*" (mk_lambda ["a0"; "a1"] (mk_prim_app P_Mult ["a0"; "a1"]));
    mk_define "-" (mk_lambda ["a0"; "a1"] (mk_prim_app P_Minus ["a0"; "a1"]));
    mk_define "/" (mk_lambda ["a0"; "a1"] (mk_prim_app P_Div ["a0"; "a1"]));

    (* Comparison primitives *)
    mk_define "=" (mk_lambda ["a0"; "a1"] (mk_prim_app P_Eq ["a0"; "a1"]));
    mk_define "eq?" (mk_lambda ["a0"; "a1"] (mk_prim_app P_Eq ["a0"; "a1"]));
    mk_define "!=" (mk_lambda ["a0"; "a1"] (mk_prim_app P_Ne ["a0"; "a1"]));
    mk_define "<" (mk_lambda ["a0"; "a1"] (mk_prim_app P_Lt ["a0"; "a1"]));
    mk_define ">" (mk_lambda ["a0"; "a1"] (mk_prim_app P_Gt ["a0"; "a1"]));
    mk_define "<=" (mk_lambda ["a0"; "a1"] (mk_prim_app P_Le ["a0"; "a1"]));
    mk_define ">=" (mk_lambda ["a0"; "a1"] (mk_prim_app P_Ge ["a0"; "a1"]));

    (* List primitives *)
    mk_define "cons" (mk_lambda ["a0"; "a1"] (mk_prim_app P_Cons ["a0"; "a1"]));
    mk_define "car" (mk_lambda ["a0"] (mk_prim_app P_Car ["a0"]));
    mk_define "cdr" (mk_lambda ["a0"] (mk_prim_app P_Cdr ["a0"]));

    (* Constants *)
    mk_define "nil" (syn E_Nil);

    (* Predicates *)
    mk_define "nil?" (mk_lambda ["a0"] (mk_prim_app P_IsNil ["a0"]));
    mk_define "pair?" (mk_lambda ["a0"] (mk_prim_app P_IsPair ["a0"]));
    mk_define "number?" (mk_lambda ["a0"] (mk_prim_app P_IsNumber ["a0"]));

    (* Logical primitives *)
    mk_define "and" (mk_lambda ["a0"; "a1"] (mk_prim_app P_And ["a0"; "a1"]));
    mk_define "or" (mk_lambda ["a0"; "a1"] (mk_prim_app P_Or ["a0"; "a1"]));
    mk_define "not" (mk_lambda ["a0"] (mk_prim_app P_Not ["a0"]));
  ]
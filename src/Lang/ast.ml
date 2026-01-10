type location = { file : string; line : int; column : int }
type prim = Builtins.prim

(* Literal values *)
type lit =
  | L_Bool of bool
  | L_Number of int
  | L_String of string

type top_expr_data = E_Expr of expr | E_Define of var * expr
and top_expr = top_expr_data
and var = string

and expr_data =
  | E_Lit of lit
  | E_Var of var
  | E_App of expr * expr list
  | E_Lambda of var list * expr
  | E_If of expr * expr * expr
  | E_Callcc of var * expr
  | E_Prim of prim

and expr = expr_data

(* Stringifies the AST *)
let string_of_expr expr =
  let rec string_of_expr_aux offset expr =
    match expr with
    | E_Lit (L_Bool b) -> string_of_bool b
    | E_Lit (L_Number n) -> string_of_int n
    | E_Lit (L_String s) -> "\"" ^ s ^ "\""
    | E_Var s -> s
    | E_App (func, args) ->
        "("
        ^ string_of_expr_aux offset func
        ^ " "
        ^ String.concat " " (List.map (string_of_expr_aux (offset + 2)) args)
        ^ ")"
    | E_Lambda (ids, body) ->
        let indent = String.make offset ' ' in
        "(lambda (" ^ String.concat " " ids ^ ")\n" ^ indent ^ "  "
        ^ string_of_expr_aux (offset + 2) body
        ^ ")"
    | E_If (cond, y, n) ->
        let indent = String.make offset ' ' in
        "(if "
        ^ string_of_expr_aux offset cond
        ^ "\n" ^ indent ^ "    "
        ^ string_of_expr_aux (offset + 4) y
        ^ "\n" ^ indent ^ "    "
        ^ string_of_expr_aux (offset + 4) n
        ^ ")"
    | E_Callcc (v, e) -> "(callcc " ^ v ^ ". " ^  string_of_expr_aux (offset + 2) e ^ ")"
    | E_Prim prim -> "<primitive: \"" ^ Builtins.builtin_to_string prim ^ "\">"
  in
  string_of_expr_aux 0 expr

let string_of_top_expr top_expr =
  match top_expr with
  | E_Expr e -> string_of_expr e ^ "\n"
  | E_Define (name, expr) ->
      "(define " ^ name ^ "\n  " ^ string_of_expr expr ^ ")\n"

(* Helper functions for creating builtin definitions *)
let mk_var name = E_Var name
let mk_lambda params body = E_Lambda (params, body)

let mk_prim_app prim vars =
  E_App (E_Prim prim, List.map mk_var vars)

let mk_define name expr = E_Define (name, expr)

(* Builtin primitive definitions as AST expressions *)
let builtin_definitions : top_expr list =
  [
    (* Arithmetic primitives *)
    mk_define "+"
      (mk_lambda [ "a0"; "a1" ] (mk_prim_app Builtins.P_Plus [ "a0"; "a1" ]));
    mk_define "*"
      (mk_lambda [ "a0"; "a1" ] (mk_prim_app Builtins.P_Mult [ "a0"; "a1" ]));
    mk_define "-"
      (mk_lambda [ "a0"; "a1" ] (mk_prim_app Builtins.P_Minus [ "a0"; "a1" ]));
    mk_define "/"
      (mk_lambda [ "a0"; "a1" ] (mk_prim_app Builtins.P_Div [ "a0"; "a1" ]));
    (* Comparison primitives *)
    mk_define "="
      (mk_lambda [ "a0"; "a1" ] (mk_prim_app Builtins.P_Eq [ "a0"; "a1" ]));
    mk_define "eq?"
      (mk_lambda [ "a0"; "a1" ] (mk_prim_app Builtins.P_Eq [ "a0"; "a1" ]));
    mk_define "!="
      (mk_lambda [ "a0"; "a1" ] (mk_prim_app Builtins.P_Ne [ "a0"; "a1" ]));
    mk_define "<"
      (mk_lambda [ "a0"; "a1" ] (mk_prim_app Builtins.P_Lt [ "a0"; "a1" ]));
    mk_define ">"
      (mk_lambda [ "a0"; "a1" ] (mk_prim_app Builtins.P_Gt [ "a0"; "a1" ]));
    mk_define "<="
      (mk_lambda [ "a0"; "a1" ] (mk_prim_app Builtins.P_Le [ "a0"; "a1" ]));
    mk_define ">="
      (mk_lambda [ "a0"; "a1" ] (mk_prim_app Builtins.P_Ge [ "a0"; "a1" ]));
    (* List primitives *)
    mk_define "pair"
      (mk_lambda [ "a0"; "a1" ] (mk_prim_app Builtins.P_pair [ "a0"; "a1" ]));
    mk_define "fst" (mk_lambda [ "a0" ] (mk_prim_app Builtins.P_fst [ "a0" ]));
    mk_define "snd" (mk_lambda [ "a0" ] (mk_prim_app Builtins.P_snd [ "a0" ]));
    (* Predicates *)
    mk_define "nil?"
      (mk_lambda [ "a0" ] (mk_prim_app Builtins.P_IsNil [ "a0" ]));
    mk_define "pair?"
      (mk_lambda [ "a0" ] (mk_prim_app Builtins.P_IsPair [ "a0" ]));
    mk_define "list?"
      (mk_lambda [ "a0" ] (mk_prim_app Builtins.P_IsList [ "a0" ]));
    mk_define "number?"
      (mk_lambda [ "a0" ] (mk_prim_app Builtins.P_IsNumber [ "a0" ]));
    (* Logical primitives *)
    mk_define "and"
      (mk_lambda [ "a0"; "a1" ] (mk_prim_app Builtins.P_And [ "a0"; "a1" ]));
    mk_define "or"
      (mk_lambda [ "a0"; "a1" ] (mk_prim_app Builtins.P_Or [ "a0"; "a1" ]));
    mk_define "not" (mk_lambda [ "a0" ] (mk_prim_app Builtins.P_Not [ "a0" ]));
    (* Print primitive *)
    mk_define "print"
      (mk_lambda [ "a0" ] (mk_prim_app Builtins.P_Print [ "a0" ]));
    (* Mutation primitive *)
    mk_define "set!"
      (mk_lambda [ "cell"; "value" ]
         (mk_prim_app Builtins.P_Set [ "cell"; "value" ]));
    (* Box operations *)
    mk_define "box" (mk_lambda [ "a0" ] (mk_prim_app Builtins.P_Box [ "a0" ]));
    mk_define "unwrap"
      (mk_lambda [ "a0" ] (mk_prim_app Builtins.P_Unwrap [ "a0" ]));
    mk_define "peek" (mk_lambda [ "a0" ] (mk_prim_app Builtins.P_Peek [ "a0" ]));
  ]

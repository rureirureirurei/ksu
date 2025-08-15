type location = { file : string; line : int; column : int }

type prim = P_Car | P_Cdr | P_Cons | P_IsNil | P_IsPair | P_IsNumber | P_Plus | P_Minus | P_Mult | P_Div | P_Mod | P_Eq | P_Ne | P_Lt | P_Le | P_Gt | P_Ge | P_And | P_Or | P_Not

type top_expr_data = E_Expr of expr | E_Define of var * expr

and top_expr = top_expr_data node
and var = string

and expr_data =
  | E_Bool of bool
  | E_Number of int
  | E_String of string
  | E_Var of var
  | E_App of expr * expr list
  | E_Lambda of var list * expr
  | E_If of expr * expr * expr
  | E_Callcc of expr
  | E_Let of (var * expr) list * expr
  | E_Pair of expr * expr
  | E_Nil
  | E_Prim of prim

and expr = expr_data node
and 'a node = { value : 'a;  loc : location }

let string_of_prim = function
          | P_Car -> "car"
          | P_Cdr -> "cdr"
          | P_Cons -> "cons"
          | P_IsNil -> "is-nil"
          | P_IsPair -> "is-pair"
          | P_IsNumber -> "is-number"
          | P_Plus -> "+"
          | P_Minus -> "-"
          | P_Mult -> "*"
          | P_Div -> "/"
          | P_Mod -> "mod"
          | P_Eq -> "="
          | P_Ne -> "!="
          | P_Lt -> "<"
          | P_Le -> "<="
          | P_Gt -> ">"
          | P_Ge -> ">="
          | P_And -> "and"
          | P_Or -> "or"
          | P_Not -> "not"

(* Stringifies the AST *)
let string_of_expr expr =
  let rec string_of_expr_aux offset { value; _ } =
    match value with
      | E_Bool b -> string_of_bool b
    | E_Number n -> string_of_int n
    | E_String s -> "\"" ^ s ^ "\""
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
    | E_Callcc e -> "(callcc " ^ string_of_expr_aux (offset + 2) e ^ ")"
    | E_Let (defs, body) ->
        let indent = String.make offset ' ' in
        let defs_str =
          List.map
            (fun (name, expr) ->
              "[" ^ name ^ " " ^ string_of_expr_aux (offset + 4) expr ^ "]")
            defs
        in
        "(let ("
        ^ String.concat ("\n" ^ indent ^ "   ") defs_str
        ^ ")\n" ^ indent ^ "  "
        ^ string_of_expr_aux (offset + 2) body
        ^ ")"
    | E_Pair (e1, e2) ->
        "(cons "
        ^ string_of_expr_aux offset e1
        ^ " "
        ^ string_of_expr_aux offset e2
        ^ ")"
    | E_Nil -> "nil"
    | E_Prim prim -> "<primitive: \"" ^ string_of_prim prim ^ "\">"
  in
  string_of_expr_aux 0 expr

let string_of_top_expr top_expr =
  match top_expr.value with
  | E_Expr e -> string_of_expr e ^ "\n"
  | E_Define (name, expr) ->
      "(define " ^ name ^ "\n  " ^ string_of_expr expr ^ ")\n"



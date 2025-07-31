type top_expr = Expr of expr | Define of { name : string; expr : expr }

and expr =
  | Bool of bool
  | Number of int
  | String of string
  | Symbol of string
  | App of expr list
  | Lambda of { ids : string list; body : expr }
  | If of { cond : expr; y : expr; n : expr }
  | Callcc of expr
  | Let of { defs : (string * expr) list; body : expr }
  | Pair of expr * expr
  | Nil

let rec string_of_expr = function
  | Bool b -> string_of_bool b
  | Number n -> string_of_int n
  | String s -> s
  | Symbol s -> s
  | App l -> "(" ^ String.concat "\n  " (List.map string_of_expr l) ^ ")"
  | Lambda { ids; body } ->
      "(lambda (" ^ String.concat " " ids ^ ")\n  " ^ string_of_expr body ^ ")"
  | If { cond; y; n } ->
      "(if " ^ string_of_expr cond ^ "\n    " ^ string_of_expr y ^ "\n    "
      ^ string_of_expr n ^ ")"
  | Callcc e -> "(callcc " ^ string_of_expr e ^ ")"
  | Let { defs; body } ->
      "(let ("
      ^ String.concat " " (List.map fst defs)
      ^ ")\n  ("
      ^ String.concat "\n   " (List.map string_of_expr (List.map snd defs))
      ^ ")\n  " ^ string_of_expr body ^ ")"
  | Pair (e1, e2) ->
      "(cons " ^ string_of_expr e1 ^ " :: " ^ string_of_expr e2 ^ ")"
  | Nil -> "nil"

and string_of_top_expr = function
  | Expr e -> string_of_expr e
  | Define { name; expr } ->
      "(define " ^ name ^ "\n  " ^ string_of_expr expr ^ ")"

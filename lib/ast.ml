type expr =
  | Bool of bool
  | Number of int
  | String of string
  | Symbol of string
  | App of expr list
  | Lambda of { ids : string list; body : expr }
  | If of { cond : expr; y : expr; n : expr }
  | Callcc of expr
  | Let of { ids : string list; defs : expr list; b : expr }
  | Define of { name : string; expr : expr }

let rec string_of_expr = function
  | Bool b -> string_of_bool b
  | Number n -> string_of_int n
  | String s -> s
  | Symbol s -> s
  | App l -> 
      "(" ^ String.concat "\n  " (List.map string_of_expr l) ^ ")"
  | Lambda { ids; body } ->
      "(lambda (" ^ String.concat " " ids ^ ")\n  " ^ string_of_expr body ^ ")"
  | If { cond; y; n } ->
      "(if " ^ string_of_expr cond ^ "\n    " ^ string_of_expr y ^ "\n    " ^ string_of_expr n ^ ")"
  | Callcc e -> "(callcc " ^ string_of_expr e ^ ")"
  | Let { ids; defs; b } ->
      "(let (" ^ String.concat " " ids ^ ")\n  (" ^ 
      String.concat "\n   " (List.map string_of_expr defs) ^
      ")\n  " ^ string_of_expr b ^ ")"
  | Define { name; expr } -> 
      "(define " ^ name ^ "\n  " ^ string_of_expr expr ^ ")"

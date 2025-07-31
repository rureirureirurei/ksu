
type location = {
  file: string;
  line: int;
  column: int;
}

type top_expr_data = Expr of expr | Define of { name : string; expr : expr }

and top_expr = top_expr_data node

and expr_data =
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

and expr = expr_data node

and 'a node = {
  value : 'a;
  id: int;
  loc: location;
}

let fresh_node_tag: unit -> int =
  let counter = ref 0 in
  fun () ->
    let result = !counter in
    counter := result + 1;
    result

let rec string_of_expr {value; _} = match value with
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

let string_of_top_expr {value; _} = match value with
  | Expr e -> string_of_expr e
  | Define { name; expr } ->
      "(define " ^ name ^ "\n  " ^ string_of_expr expr ^ ")"

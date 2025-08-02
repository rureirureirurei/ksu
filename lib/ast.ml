type location = { file : string; line : int; column : int }

type top_expr_data = Expr of expr | Define of { name : string; expr : expr }
and top_expr = top_expr_data node
and var = string

and expr_data =
  | Bool of bool
  | Number of int
  | String of string
  | Var of var
  | App of expr list
  | Lambda of { ids : var list; body : expr }
  | If of { cond : expr; y : expr; n : expr }
  | Callcc of expr
  | Let of { defs : (var * expr) list; body : expr }
  | Pair of expr * expr
  | Nil

and expr = expr_data node
and 'a node = { value : 'a; id : int; loc : location }

let fresh_node_tag : unit -> int =
  let counter = ref 0 in
  fun () ->
    let result = !counter in
    counter := result + 1;
    result

let synthetic : expr_data -> expr =
 fun data ->
  {
    value = data;
    id = fresh_node_tag ();
    loc = { file = ""; line = 0; column = 0 };
  }

let fresh_var : unit -> (expr * var) =
  let counter = ref 0 in
  fun () ->
    let result = "synthetic_var_" ^ string_of_int !counter in
    counter := !counter + 1;
    (synthetic (Var result), result)

(* Takes OCaml list and returns list in the AST representation *)
let rec genlist : expr list -> expr =
 fun l ->
  match l with
  | [] -> synthetic Nil
  | x :: xs -> synthetic (Pair (x, genlist xs))

(* Stringifies the AST *)
let string_of_expr expr =
  let rec string_of_expr_aux offset { value; _ } =
    match value with
    | Bool b -> string_of_bool b
    | Number n -> string_of_int n
    | String s -> s
    | Var s -> s
    | App l ->
        let indent = String.make offset ' ' in
        let args_str = List.map (string_of_expr_aux (offset + 2)) l in
        "(" ^ String.concat ("\n" ^ indent ^ "  ") args_str ^ ")"
    | Lambda { ids; body } ->
        let indent = String.make offset ' ' in
        "(lambda (" ^ String.concat " " ids ^ ")\n" ^ indent ^ "  "
        ^ string_of_expr_aux (offset + 2) body
        ^ ")"
    | If { cond; y; n } ->
        let indent = String.make offset ' ' in
        "(if "
        ^ string_of_expr_aux offset cond
        ^ "\n" ^ indent ^ "    "
        ^ string_of_expr_aux (offset + 4) y
        ^ "\n" ^ indent ^ "    "
        ^ string_of_expr_aux (offset + 4) n
        ^ ")"
    | Callcc e -> "(callcc " ^ string_of_expr_aux (offset + 2) e ^ ")"
    | Let { defs; body } ->
        let indent = String.make offset ' ' in
        let defs_str =
          List.map
            (fun (name, expr) ->
              name ^ " " ^ string_of_expr_aux (offset + 4) expr)
            defs
        in
        "(let ("
        ^ String.concat " " (List.map fst defs)
        ^ ")\n" ^ indent ^ "  ("
        ^ String.concat ("\n" ^ indent ^ "   ") defs_str
        ^ ")\n" ^ indent ^ "  "
        ^ string_of_expr_aux (offset + 2) body
        ^ ")"
    | Pair (e1, e2) ->
        "(cons "
        ^ string_of_expr_aux offset e1
        ^ " "
        ^ string_of_expr_aux offset e2
        ^ ")"
    | Nil -> "nil"
  in
  string_of_expr_aux 0 expr

let string_of_top_expr { value; _ } =
  match value with
  | Expr e -> string_of_expr e
  | Define { name; expr } ->
      "(define " ^ name ^ "\n  " ^ string_of_expr expr ^ ")"

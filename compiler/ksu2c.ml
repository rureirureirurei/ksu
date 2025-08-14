(* Translates Ksu to C *)
open Compiler_lib

type c_expr = 
| Block of c_expr list
| Apply of { func: string; args: c_expr list}
| Assign of { lhs: string; rhs: c_expr }
| Var of string
| Bool of bool
| Int of int
| String of string
| Nil
| Return of c_expr

type top_c_expr = 
| FuncDef of { name: string; args: string list; body: c_expr }
| VarDef of { name: string; expr: c_expr }

let rec ksu2c_top: Ast.top_expr list -> top_c_expr list -> top_c_expr list =
 fun exprs incoming_globals ->
  let (main_body: c_expr list), (defs: top_c_expr list), (globals: top_c_expr list) =
    List.fold_left
      (fun (main_body, defs, globals) (expr: Ast.top_expr) ->
        match expr.value with
        | Ast.Define { name; expr } -> (
            match expr.value with
            | Ast.Lambda { ids; body } ->
                let body_c, globals = ksu2c body globals in
                ( main_body, defs, globals @ [ FuncDef { name; args = ids; body = body_c } ] )
            | _ ->
                let expr_c, globals = ksu2c expr globals in
                ( main_body, defs, globals @ [ VarDef { name; expr = expr_c } ] )
          )
        | Ast.Expr expr ->
            let _, fresh_fun_id = Ast.fresh_var () in
            let body_c, globals = ksu2c expr globals in
            let main_call = Apply { func = fresh_fun_id; args = [] } in
            let func = FuncDef { name = fresh_fun_id; args = []; body = body_c } in
            ( main_body @ [ main_call ], func :: defs, globals )
      )
      ([], [], incoming_globals) exprs
  in
  let main_func = FuncDef { name = "main"; args = []; body = Block main_body } in
  globals @ List.rev defs @ [ main_func ]

and prim_to_c_function_name: string -> string = function 
| "cons" -> "MakePair"
| "pair?" -> "is_cons"
| "cons?" -> "is_cons"
| "null?" -> "is_null"
| "+" -> "Add"
| "-" -> "Sub"
| "*" -> "Mul"
| "/" -> "Div"
| "<" -> "Lt"
| ">" -> "Gt"
| "<=" -> "Le"
| ">=" -> "Ge"
| "boolean?" -> "is_bool"
| "number?" -> "is_int"
| "lambda?" -> "is_lambda"
| "list?" -> "is_list"
| "=" -> "Eq"
| "eq?" -> "Eq"
| "list-ref" -> "ListRef"
| p -> failwith ("Unsupported primitive: " ^ p)

and ksu2c: Ast.expr -> top_c_expr list -> c_expr * top_c_expr list =
 fun expr globals -> match expr.value with 
| Bool b -> (Bool b, globals)
| Number i -> (Int i, globals)
| String s -> (String s, globals)
| Nil -> (Nil, globals)
| Var s -> (Var s, globals)
| App { func; args } -> 
    let func_c, globals = ksu2c func globals in
    let args_c, globals = List.fold_left (fun (acc, g) a -> let a_c, g' = ksu2c a g in (acc @ [a_c], g')) ([], globals) args in
    (match func_c with 
    | Var fname -> (Apply { func=fname; args=args_c }, globals)
    | _ -> failwith "Only direct function names are supported in application for now"
    )
| If { cond; y; n } -> 
    let cond_c, g1 = ksu2c cond globals in
    let y_c, g2 = ksu2c y g1 in
    let n_c, g3 = ksu2c n g2 in
    (Apply { func = "Cond"; args = [ cond_c; y_c; n_c ] }, g3)
| Let { defs; body } ->
    let fresh_name = snd (Ast.fresh_var ()) in
    (* translate defs to values and collect their names/values *)
    let arg_names, arg_values, g_after_defs =
      List.fold_left
        (fun (names, values, g) (id, e) ->
          let e_c, g' = ksu2c e g in
          (names @ [ id ], values @ [ e_c ], g'))
        ([], [], globals) defs
    in
    let body_c, g_after_body = ksu2c body g_after_defs in
    let func_def = FuncDef { name = fresh_name; args = arg_names; body = Return body_c } in
    (Apply { func = fresh_name; args = arg_values }, g_after_body @ [ func_def ])
| Pair (e1, e2) -> 
    let e1_c, g1 = ksu2c e1 globals in
    let e2_c, g2 = ksu2c e2 g1 in
    (Apply { func = "MakePair"; args = [ e1_c; e2_c ] }, g2)
| Prim p -> (Var (prim_to_c_function_name p), globals)
| Car e -> let e_c, g = ksu2c e globals in (Apply { func = "Car"; args = [ e_c ] }, g)
| Cdr e -> let e_c, g = ksu2c e globals in (Apply { func = "Cdr"; args = [ e_c ] }, g)
| Callcc _ -> failwith "Callcc not implemented"
| Lambda _ -> failwith "Lambdas expected only on the top level"

and ast2text: top_c_expr list -> string = function 
| FuncDef _ as func :: rest -> func2text func ^ "\n" ^ ast2text rest
| VarDef _ as def :: rest -> def2text def ^ "\n" ^ ast2text rest
| [] -> ""

and func2text: top_c_expr -> string = function 
| FuncDef {name; args; body} -> 
  let args_str = String.concat ", " (List.map (fun arg -> "Value " ^ arg) args) in 
  let body_str = c_expr2text body in 
  "Value " ^ name ^ "(" ^ args_str ^ ") {\n" ^ body_str ^ "\n}"
| _ -> failwith "func2text expected FuncDef"

and c_expr2text: c_expr -> string = function 
| Block exprs -> String.concat "\n" (List.map c_expr2text exprs)
| Apply {func; args} -> func ^ "(" ^ String.concat ", " (List.map (fun arg -> "(" ^ (c_expr2text arg) ^ ")") args) ^ ")"
| Assign {lhs; rhs} -> lhs ^ " = " ^ c_expr2text rhs ^ ";"
| Var name -> name
| Bool b -> let b_str = if b then "true" else "false" in "MakeBool(" ^ b_str ^ ")"
| Int i -> "MakeInt(" ^ (string_of_int i) ^ ")"
| String _ -> failwith "c_expr2text :: todo strings are not implemented yet"
| Nil -> "MakeNil()"
| Return e -> "return " ^ c_expr2text e ^ ";"

and def2text: top_c_expr -> string = function 
| VarDef {name; expr} -> "Value " ^ name ^ " = " ^ c_expr2text expr
| _ -> "def2text expected variable definition"


and top_exprs2c: Ast.top_expr list -> string = fun exprs ->
  let c_ast = ksu2c_top exprs [] in
  Ksu2c_header.header ^ "\n\n" ^ ast2text c_ast
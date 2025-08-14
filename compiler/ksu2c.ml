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

type top_c_expr = 
| FuncDef of { name: string; args: string list; body: c_expr }
| VarDef of { name: string; expr: c_expr }

let rec ksu2c_top: Ast.top_expr list -> top_c_expr list = fun exprs -> 
  let (main_body: c_expr list), (defs: top_c_expr list) = List.fold_left_map 
    (fun (main_body: c_expr list) (expr: Ast.top_expr) -> 
      match expr.value with 
      | Ast.Define { name; expr} -> (
        match expr.value with 
        | Ast.Lambda {ids; body} -> main_body, FuncDef { name=name; args=ids;body=(ksu2c body)}
        | _ -> main_body, VarDef {name=name; expr=(ksu2c expr)}
      )
      | Ast.Expr expr -> (
        let _, fresh_fun_id = Ast.fresh_var () in 
        let main_call = Apply { func=fresh_fun_id; args=[] } in 
        let func = FuncDef { name=fresh_fun_id; args=[]; body=(ksu2c expr)} in 
        (main_body @ [main_call]), func
      )
    ) 
    [] exprs 
  in 
  let main_func = FuncDef {name="main";args=[];body=(Block main_body)}
in defs @ [main_func]

and ksu2c: Ast.expr -> c_expr = fun expr -> match expr.value with 
| Bool b -> Bool b
| Number i -> Int i
| String s -> String s
| _ -> failwith "Other cases translation to C is not yet implemented"

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

and def2text: top_c_expr -> string = function 
| VarDef {name; expr} -> "Value " ^ name ^ " = " ^ c_expr2text expr
| _ -> "def2text expected variable definition"


and top_exprs2c: Ast.top_expr list -> string = fun exprs ->
  ast2text (ksu2c_top exprs)
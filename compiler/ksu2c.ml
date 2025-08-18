open Compiler_lib
open Closures
open Ast

let p2c: Ast.prim -> string = function
| P_And -> "and"
| P_Car -> "car"
| P_Cdr -> "cdr"
| P_Cons -> "cons"
| P_IsNil -> "is_null"
| P_IsPair -> "is_cons"
| P_IsNumber -> "is_int"
| P_Plus -> "Add"
| P_Minus -> "Sub"
| P_Mult -> "Mul"
| P_Div -> "Div"
| P_Or -> "or"
| P_Not -> "not"
| P_Eq -> "eq"
| P_Ne -> "ne"
| P_Lt -> "Lt"
| P_Le -> "Le"
| P_Gt -> "Gt"
| P_Ge -> "Ge"

let ksu2c: Closures.cc_top_expr list -> string = 
  let gen_fresh_var domain =
    let num = ref 0 in 
    (fun () -> num := !num + 1; (domain ^ "_" ^ (string_of_int !num))) in
  let gen_main_expr = gen_fresh_var "MainExpr" in 
  let gen_tmp = gen_fresh_var "tmp" in

  let res: string list ref = ref [] in 

  let main_body_exprs: string list ref = ref [] in 
  let closure_envs: string list ref = ref [] in

  let rec string_of_cc_expr: cc_expr -> string = function
  | CC_Number n -> "MakeInt(" ^ string_of_int n ^ ")"
  | CC_Bool b -> "MakeBool(" ^ string_of_bool b ^ ")"
  | CC_String s -> "MakeString(" ^ s ^ ")"
  | CC_If (c, y, n) -> "(" ^ (string_of_cc_expr c) ^ " ? " ^ (string_of_cc_expr y) ^ " : " ^ (string_of_cc_expr n) ^ ")" 
  | CC_Var v -> v
  (*C Closures *)
  | CC_MakeClosure (var, c_expr) -> "MakeClosure((Lambda)" ^ var ^ ", " ^ string_of_cc_expr c_expr ^ ")"
  | CC_AppClosure (fn, args) -> 
      let tmp_var = gen_tmp () in
      "({ Value " ^ tmp_var ^ " = " ^ string_of_cc_expr fn ^ "; " ^ 
      tmp_var ^ ".clo.lam(" ^ tmp_var ^ ".clo.env" ^ 
      (if List.length args > 0 then ", " else "") ^ 
      String.concat ", " (List.map string_of_cc_expr args) ^ "); })"
  | CC_MakeEnv (vars, env_struct_id) -> "alloc_" ^ env_struct_id ^ "(" ^ (String.concat "," vars) ^ ")"
  | CC_EnvRef (env, var) -> "EnvRef(" ^ string_of_cc_expr env ^ ", " ^ var ^ ")"
  | CC_Let (defs, body) ->
    let def_exprs = String.concat "" (List.map (fun (v, expr) -> "  Value " ^ v ^ " = " ^ string_of_cc_expr expr ^ ";\n") defs) in
    "({\n" ^ def_exprs ^ string_of_cc_expr body ^ ";\n })"
  | CC_Nil -> "MakeNil()"
  | CC_Callcc _ -> failwith "todo"
  | CC_Pair (e1, e2) -> "MakePair(" ^ string_of_cc_expr e1 ^ ", " ^ string_of_cc_expr e2 ^ ")"
  | CC_PrimApp (p, args) -> p2c p ^ "(" ^ String.concat ", " args ^ ")"

  and t_top: cc_top_expr -> unit = fun top_expr -> match top_expr with 
  | CC_FuncDef  (name, args, body) -> 
    let args_str = String.concat ", " (List.map (fun arg -> "Value " ^ arg) args) in 
    res := ("Value " ^ name ^ "(" ^ args_str ^ ") {\nreturn " ^ (string_of_cc_expr body) ^ ";\n}") :: !res
  | CC_VarDef (name, e) -> res := ("Value " ^ name ^ " = " ^ (string_of_cc_expr e) ^ ";") :: !res
  | CC_Expr e -> main_body_exprs := !main_body_exprs @ [string_of_cc_expr e]
  | CC_EnvDef (envid, vars) -> closure_envs := !closure_envs @ [string_of_env envid vars]

  (* struct definition and function for allocating env *)
  and string_of_env: var -> var list -> string = fun id vars -> 
    let struct_def = 
      "struct " ^ id ^ " {\n" ^
        String.concat "\n" (List.map (fun v -> "  Value " ^ v ^ ";") vars) ^
      "\n};" in
    
    let alloc_params = 
      if List.length vars = 0 then "" 
      else "Value " ^ String.concat ", Value " vars in
    
    let alloc_body = 
      if List.length vars = 0 then
        "return MakeEnv(NULL);"
      else
        "struct " ^ id ^ "* t = malloc(sizeof(struct " ^ id ^ "));\n" ^
        String.concat "\n" (List.map (fun v -> "  t->" ^ v ^ " = " ^ v ^ ";") vars) ^
        "\n  return MakeEnv(t);" in
    
    let alloc_func = 
      "Value alloc_" ^ id ^ "(" ^ alloc_params ^ ") {\n" ^
      "  " ^ alloc_body ^ "\n}" in
    
    struct_def ^ "\n\n" ^ alloc_func

in fun exprs -> 
  List.iter t_top exprs;
  let main_exprs = List.map (fun e -> "Value " ^ (gen_main_expr ()) ^ " = " ^ e ^ ";") !main_body_exprs in 
  let main_body = "int main() {\n" ^ (String.concat "\n" main_exprs) ^ "\n}" in 
  Ksu2c_header.header ^ "\n\n" ^ String.concat "\n\n" (!closure_envs @ !res @ [main_body])
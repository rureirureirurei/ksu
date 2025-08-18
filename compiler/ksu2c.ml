open Compiler_lib
open Closures
open Ast

let p2c: Ast.prim -> string = function
| P_And -> "__builtin_and"
| P_Car -> "__builtin_car"
| P_Cdr -> "__builtin_cdr"
| P_Cons -> "__builtin_cons"
| P_IsNil -> "__builtin_is_null"
| P_IsPair -> "__builtin_is_cons"
| P_IsNumber -> "__builtin_is_int"
| P_Plus -> "__builtin_add"
| P_Minus -> "__builtin_sub"
| P_Mult -> "__builtin_mul"
| P_Div -> "__builtin_div"
| P_Or -> "__builtin_or"
| P_Not -> "__builtin_not"
| P_Print -> "__builtin_print"
| P_Eq -> "__builtin_eq"
| P_Ne -> "__builtin_ne"
| P_Lt -> "__builtin_lt"
| P_Le -> "__builtin_le"
| P_Gt -> "__builtin_gt"
| P_Ge -> "__builtin_ge"

let ksu2c: Closures.cc_top_expr list -> string = 
  let gen_fresh_var domain =
    let num = ref 0 in 
    (fun () -> num := !num + 1; (domain ^ "_" ^ (string_of_int !num))) in
  let gen_main_expr = gen_fresh_var "MainExpr" in 
  let gen_tmp = gen_fresh_var "tmp" in

  (* Helper function to generate cast type for function pointers *)
  let gen_cast_type arg_count =
    let value_params = "Value" ^ String.concat ", Value" (List.init (arg_count + 1) (fun _ -> "")) in
    "(Value (*)(" ^ value_params ^ "))" in

  let res: string list ref = ref [] in 
  let global_decls: string list ref = ref [] in
  let global_inits: string list ref = ref [] in

  let main_body_exprs: string list ref = ref [] in 
  let closure_envs: string list ref = ref [] in

  let rec string_of_cc_expr: cc_expr -> string = function
  | CC_Number n -> "MakeInt(" ^ string_of_int n ^ ")"
  | CC_Bool b -> "MakeBool(" ^ string_of_bool b ^ ")"
  | CC_String s -> "MakeString(\"" ^ s ^ "\")"
  | CC_If (c, y, n) -> "(__builtin_is_true(" ^ (string_of_cc_expr c) ^ ") ? " ^ (string_of_cc_expr y) ^ " : " ^ (string_of_cc_expr n) ^ ")" 
  | CC_Var v -> v
  (*C Closures *)
  | CC_MakeClosure (var, c_expr) -> "MakeClosure((Lambda)" ^ var ^ ", " ^ string_of_cc_expr c_expr ^ ")"
  | CC_AppClosure (fn, args) -> 
      let tmp_var = gen_tmp () in
      let args_str = String.concat ", " (List.map string_of_cc_expr args) in
      let arg_count = List.length args in
      let cast_type = gen_cast_type arg_count in
      "({ Value " ^ tmp_var ^ " = " ^ string_of_cc_expr fn ^ "; " ^ 
      "(" ^ cast_type ^ tmp_var ^ ".clo.lam)(MakeEnv(" ^ tmp_var ^ ".clo.env)" ^ 
      (if List.length args > 0 then ", " else "") ^ 
      args_str ^ "); })"
  | CC_MakeEnv (vars, env_struct_id) -> "alloc_" ^ env_struct_id ^ "(" ^ (String.concat "," vars) ^ ")"
  | CC_EnvRef (env, var) -> "EnvRef(" ^ string_of_cc_expr env ^ ", " ^ var ^ ")"
  | CC_Let (defs, body) ->
    let def_exprs = String.concat "" (List.map (fun (v, expr) -> "  Value " ^ v ^ " = " ^ string_of_cc_expr expr ^ ";\n") defs) in
    "({\n" ^ def_exprs ^ string_of_cc_expr body ^ ";\n })"
  | CC_Nil -> "MakeNil()"
  | CC_Callcc _ -> failwith "todo"
  | CC_Pair (e1, e2) -> "MakePair(" ^ string_of_cc_expr e1 ^ ", " ^ string_of_cc_expr e2 ^ ")"
  | CC_PrimApp (p, args) -> p2c p ^ "(" ^ String.concat ", " (List.map string_of_cc_expr args) ^ ")"

  and t_top: cc_top_expr -> unit = fun top_expr -> match top_expr with 
  | CC_FuncDef  (name, args, body) -> 
    let args_str = String.concat ", " (List.map (fun arg -> "Value " ^ arg) args) in 
    res := ("Value " ^ name ^ "(" ^ args_str ^ ") {\nreturn " ^ (string_of_cc_expr body) ^ ";\n}") :: !res
  | CC_VarDef (name, e) -> 
    global_decls := !global_decls @ [("Value " ^ name ^ ";")];
    global_inits := (name ^ " = " ^ string_of_cc_expr e ^ ";") :: !global_inits
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
  let main_body = "int main() {\n" ^ 
                  (String.concat "\n" !global_inits) ^ "\n" ^
                  (String.concat "\n" main_exprs) ^ "\n}" in 
  Ksu2c_header.header ^ "\n\n" ^ 
  (String.concat "\n" !global_decls) ^ "\n\n" ^
  String.concat "\n\n" (!closure_envs @ !res @ [main_body])
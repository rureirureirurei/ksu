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
  let gen_cast_type arg_count =
    let params =
      if arg_count = 0 then "EnvEntry*"
      else "EnvEntry*, " ^ String.concat ", " (List.init arg_count (fun _ -> "Value"))
    in
    "(Value (*)(" ^ params ^ "))"
  in

  let res: string list ref = ref [] in 
  let global_decls: string list ref = ref [] in
  let global_inits: string list ref = ref [] in

  let main_body_exprs: string list ref = ref [] in 

  let rec string_of_cc_expr: cc_expr -> string = function
  | CC_Number n -> "MakeInt(" ^ string_of_int n ^ ")"
  | CC_Bool b -> "MakeBool(" ^ string_of_bool b ^ ")"
  | CC_String s -> "MakeString(\"" ^ s ^ "\")"
  | CC_If (c, y, n) -> "(__builtin_is_true(" ^ (string_of_cc_expr c) ^ ") ? " ^ (string_of_cc_expr y) ^ " : " ^ (string_of_cc_expr n) ^ ")" 
  | CC_Var v -> v
  (*C Closures *)
  | CC_MakeClosure (var, c_expr) ->
    "MakeClosure( (Value (*)(void))" ^ var ^ ", " ^ string_of_cc_expr c_expr ^ ")"
  | CC_AppClosure (fn, args) ->
    let arg_count = List.length args in
    let args_str = String.concat ", " (List.map string_of_cc_expr args) in
    let fn_expr = string_of_cc_expr fn in
    let tmp_var = gen_tmp () in
    let cast_type = gen_cast_type arg_count in
    "({ Value " ^ tmp_var ^ " = " ^ fn_expr ^ "; (" ^ cast_type ^ tmp_var ^ ".clo.lam)(" ^ tmp_var ^ ".clo.env" ^ (if arg_count = 0 then "" else ", " ^ args_str) ^ "); })"
  | CC_MakeEnv (vars, _) ->
    let pair_count = List.length vars in
    if pair_count = 0 then
      "MakeEnv(0)"
    else (
      let decls, addr_args =
        List.fold_left (fun (decls_acc, args_acc) (name, expr) ->
          match expr with
          | CC_Var v ->
              (decls_acc, args_acc @ ["\"" ^ name ^ "\""; "&" ^ v])
          | _ ->
              let init = "  Value " ^ name ^ " = " ^ string_of_cc_expr expr ^ ";\n" in
              (decls_acc @ [init], args_acc @ ["\"" ^ name ^ "\""; "&" ^ name])
        ) ([], []) vars
      in
      let args_str = String.concat ", " addr_args in
      "({\n" ^ String.concat "" decls ^ "  MakeEnv(" ^ string_of_int pair_count ^ ", " ^ args_str ^ ");\n })"
    )
  | CC_EnvRef (env, var) ->
    "EnvRef(" ^ string_of_cc_expr env ^ ", \"" ^ var ^ "\")"
  | CC_Let (defs, body) ->
    let def_exprs = String.concat "" (List.map (fun (v, expr) -> "  Value " ^ v ^ " = " ^ string_of_cc_expr expr ^ ";\n") defs) in
    "({\n" ^ def_exprs ^ string_of_cc_expr body ^ ";\n })"
  | CC_Nil -> "MakeNil()"
  | CC_Callcc _ -> failwith "todo"
  | CC_Pair (e1, e2) -> "MakePair(" ^ string_of_cc_expr e1 ^ ", " ^ string_of_cc_expr e2 ^ ")"
  | CC_PrimApp (p, args) -> p2c p ^ "(" ^ String.concat ", " (List.map string_of_cc_expr args) ^ ")"

  and t_top: cc_top_expr -> unit = fun top_expr -> match top_expr with 
  | CC_FuncDef  (name, args, body) -> 
    let args_str =
      match args with
      | env :: rest when env = "$env" ->
          let rest_str = String.concat ", " (List.map (fun a -> "Value " ^ a) rest) in
          if rest = [] then "EnvEntry* " ^ env else "EnvEntry* " ^ env ^ ", " ^ rest_str
      | _ -> failwith "CC_FuncDef expected $env as a first argument"
    in 
    res := !res @ [("Value " ^ name ^ "(" ^ args_str ^ ") {\nreturn " ^ (string_of_cc_expr body) ^ ";\n}")]
  | CC_VarDef (name, e) -> 
    global_decls := !global_decls @ [("Value " ^ name ^ ";")];
    global_inits := !global_inits @ [(name ^ " = " ^ string_of_cc_expr e ^ ";") ]
  | CC_Expr e -> main_body_exprs := !main_body_exprs @ [string_of_cc_expr e]
  | CC_EnvDef _ -> ()


in fun exprs -> 
  List.iter t_top exprs;
  let main_exprs = List.map (fun e -> "Value " ^ (gen_main_expr ()) ^ " = " ^ e ^ ";") !main_body_exprs in 
  let main_body = "int main() {\n" ^ 
                  (String.concat "\n" !global_inits) ^ "\n" ^
                  (String.concat "\n" main_exprs) ^ "\n}" in 
  Ksu2c_header.header ^ "\n\n" ^ 
  (String.concat "\n" !global_decls) ^ "\n\n" ^
  String.concat "\n\n" (!res @ [main_body])
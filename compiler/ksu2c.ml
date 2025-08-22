open Compiler_lib
open Closures
open Ast

let c_func_of_prim: Ast.prim -> string = function
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
    "MakeClosure(" ^ var ^ ", " ^ string_of_cc_expr c_expr ^ ")"
  | CC_AppClosure (fn, args) -> (match fn with 
   | CC_Prim _ ->
     let args_str = String.concat ", " (List.map (fun a -> "CellValue(" ^ string_of_cc_expr a ^ ")") args) in
     (string_of_cc_expr fn) ^ "(" ^ args_str ^ ")"
   | _ -> 
     let args_str = String.concat ", " (List.map (fun a -> "CellValue(" ^ string_of_cc_expr a ^ ")") args) in
     let fn_expr = string_of_cc_expr fn in
     let tmp_var = gen_tmp () in
     let argc = string_of_int (List.length args) in
     let argv_decl = if args_str = "" then "Value* argv = NULL;" else "Value argv[] = { " ^ args_str ^ " };" in
     "({ Value " ^ tmp_var ^ " = CellValue(" ^ fn_expr ^ "); " ^ argv_decl ^ " (" ^ tmp_var ^ ".clo.lam)(" ^ tmp_var ^ ".clo.env, " ^ argc ^ ", argv); })")
  | CC_MakeEnv (vars, _) ->
    let pair_count = List.length vars in
    if pair_count = 0 then
      "MakeEnv(0)"
    else (
      let args_str =
        List.fold_left (fun args_acc (name, expr) ->
          match expr with
          | CC_Var v ->
              args_acc @ ["\"" ^ name ^ "\""; v]
          | CC_EnvRef _ ->
              args_acc @ ["\"" ^ name ^ "\"";  string_of_cc_expr expr]
          | _ -> failwith "CC_MakeEnv expected either EnvRef or Var"
        ) [] vars
      in
      "MakeEnv(" ^ string_of_int pair_count ^ ", " ^ (String.concat ", " args_str) ^ ")"
    )
  | CC_EnvRef (env, var) ->
    "EnvRef(" ^ string_of_cc_expr env ^ ", \"" ^ var ^ "\")"
  | CC_Let (defs, body) ->
    let def_exprs = String.concat "" (List.map (fun (v, expr) -> "  Value " ^ v ^ " = NewCell(CellValue(" ^ string_of_cc_expr expr ^ "));\n") defs) in
    "({\n" ^ def_exprs ^ string_of_cc_expr body ^ ";\n })"
  | CC_Nil -> "MakeNil()"
  | CC_Callcc _ -> failwith "todo"
  | CC_Pair (e1, e2) -> "MakePair(" ^ string_of_cc_expr e1 ^ ", " ^ string_of_cc_expr e2 ^ ")"
  | CC_Prim p -> (c_func_of_prim p)

  and t_top: cc_top_expr -> unit = fun top_expr -> match top_expr with 
  | CC_FuncDef  (name, args, body) -> 
    let func_src =
      match args with
      | env :: rest when env = "$env" ->
          let arity = List.length rest in
          let bindings =
            if arity = 0 then ""
            else
              String.concat "\n" (List.mapi (fun i a -> "  Value " ^ a ^ " = argv[" ^ string_of_int i ^ "];") rest) ^ "\n"
          in
          "Value " ^ name ^ "(EnvEntry* " ^ env ^ ", int argc, Value* argv) {\n" ^
          ("  if (argc != " ^ string_of_int arity ^ ") runtime_error(\"arity mismatch\");\n") ^
          bindings ^
          "  return " ^ (string_of_cc_expr body) ^ ";\n}"
      | _ -> failwith "CC_FuncDef expected $env as a first argument"
    in 
    res := !res @ [func_src]
  | CC_VarDef (name, e) -> 
    global_decls := !global_decls @ [("Value " ^ name ^ ";")];
    (match e with
     | CC_MakeClosure _ ->
        global_inits := !global_inits @ [
          (name ^ " = NewEmptyCell();");
          ("SetCell(" ^ name ^ ", " ^ string_of_cc_expr e ^ ");")
        ]
     | _ ->
        global_inits := !global_inits @ [
          (name ^ " = NewCell(CellValue(" ^ string_of_cc_expr e ^ "));")
        ])
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
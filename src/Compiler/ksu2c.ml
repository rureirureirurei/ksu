open Lang
open Ast
open Closures

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
| P_Set -> "__builtin_set"

(* Translation strategy:

   In C we have: global functions, global variable declarations, and main function.

   1. FuncDefs → global C functions
   2. VarDefs → global C variable declaration + initialization in main
   3. Top-level Exprs → execute in main

   IMPORTANT: We preserve order in main body. If source has:
     (define x 1)
     (print x)
     (define y 2)
     (print y)

   We generate:
     Value x;  // global declarations
     Value y;

     int main() {
       x = ...;  // init in order
       print(x);
       y = ...;
       print(y);
     }

   All expressions use gcc block expression extension ({ ... }).
*)

let ksu2c: cc_top_expr list -> string =
  let global_funcs = ref [] in
  let global_decls = ref [] in
  let main_body = ref [] in

  (* Translate expression to C expression string *)
  let rec t_expr (e: cc_expr): string = match e with
  (* Literals - use constructors, actual structs on stack *)
  | CC_Bool b -> "MakeBool(" ^ string_of_bool b ^ ")"
  | CC_Number n -> "MakeInt(" ^ string_of_int n ^ ")"
  | CC_String s -> "MakeString(\"" ^ String.escaped s ^ "\")"
  | CC_Nil -> "MakeEmptyList()"
  | CC_Pair (a, b) -> "__builtin_cons(" ^ t_expr a ^ ", " ^ t_expr b ^ ")"

  (* Variables *)
  | CC_Var v -> v

  (* Control flow *)
  | CC_If (c, y, n) ->
      "(is_true(" ^ t_expr c ^ ") ? " ^ t_expr y ^ " : " ^ t_expr n ^ ")"

  | CC_Begin exprs ->
      let stmts = List.map (fun e -> t_expr e ^ ";") exprs in
      "({ " ^ String.concat " " stmts ^ " })"

  | CC_Let (defs, body) ->
      let decls = List.map (fun (v, e) -> "Value " ^ v ^ " = " ^ t_expr e ^ ";") defs in
      "({ " ^ String.concat " " decls ^ " " ^ t_expr body ^ "; })"

  (* Closures *)
  | CC_MakeClosure (fn, env) ->
      "MakeClosure(" ^ fn ^ ", " ^ t_expr env ^ ")"

  | CC_MakeEnv vars ->
      let n = List.length vars in
      if n = 0 then "MakeEnv(0)"
      else
        let args = List.map (fun (name, e) ->
          let val_expr = match e with
            | CC_Var v -> "&" ^ v
            | CC_EnvRef (env, var) -> "EnvRef(" ^ env ^ ", \"" ^ var ^ "\")"
            | _ -> failwith "CC_MakeEnv: expected Var or EnvRef"
          in
          "\"" ^ name ^ "\", " ^ val_expr
        ) vars in
        "MakeEnv(" ^ string_of_int n ^ ", " ^ String.concat ", " args ^ ")"

  | CC_EnvRef (env, var) ->
      "*EnvRef(" ^ env ^ ", \"" ^ var ^ "\")"

  (* Application *)
  | CC_App (fn, args) ->
      let args_str = String.concat ", " (List.map t_expr args) in
      (match fn with
      | CC_Prim P_Set -> "not implemented"
      | CC_Prim p ->
          c_func_of_prim p ^ "(" ^ args_str ^ ")"
      | _ ->
          let fn_str = t_expr fn in
          let argc = string_of_int (List.length args) in
          let argv = if args_str = "" then "NULL" else "(Value[]){" ^ args_str ^ "}" in
          "({ Value _f = " ^ fn_str ^ "; " ^
          "_f.closure.lam(_f.closure.env, " ^ argc ^ ", " ^ argv ^ "); })")

  | CC_Prim _ -> failwith "bug: CC_Prim should be handled in CC_App"
  | CC_Callcc _ -> failwith "callcc not implemented"

  in

  (* Translate top-level expression *)
  let t_top (top: cc_top_expr): unit = match top with
  | CC_FuncDef (name, args, body) ->
      (* First arg is always $env *)
      let c_args = match args with
        | "$env" :: _ -> "EnvEntry* $env, int argc, Value* argv"
        | _ -> failwith "CC_FuncDef: expected $env as first arg"
      in
      let arg_bindings = match args with
        | "$env" :: rest ->
            List.mapi (fun i a -> "  Value " ^ a ^ " = argv[" ^ string_of_int i ^ "];") rest
        | _ -> []
      in
      let func =
        "Value " ^ name ^ "(" ^ c_args ^ ") {\n" ^
        String.concat "\n" arg_bindings ^
        (if arg_bindings <> [] then "\n" else "") ^
        "  return " ^ t_expr body ^ ";\n}"
      in
      global_funcs := !global_funcs @ [func]

  | CC_VarDef (name, expr) ->
      global_decls := !global_decls @ ["Value " ^ name ^ ";"];
      main_body := !main_body @ [name ^ " = " ^ t_expr expr ^ ";"]

  | CC_Expr e ->
      main_body := !main_body @ [t_expr e ^ ";"]
  in

  fun exprs ->
    List.iter t_top exprs;

    let header = "#include \"ksu_builtins.c\"\n\n" in
    let decls = String.concat "\n" !global_decls in
    let funcs = String.concat "\n\n" !global_funcs in
    let body = String.concat "\n  " !main_body in

    header ^ decls ^ "\n\n" ^ funcs ^ "\n\nint main() {\n  " ^ body ^ "\n}\n"

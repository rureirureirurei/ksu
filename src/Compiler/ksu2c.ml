open Lang
open Closures

(* Convert primitive to C function name *)
let prim_to_c_name p =
  let name = Builtins.builtin_to_string p in
  (* Special short names for common operators and predicates *)
  let sanitized = match name with
  | "+" -> "add"
  | "-" -> "sub"
  | "*" -> "mul"
  | "/" -> "div"
  | "=" -> "eq"
  | "!=" -> "ne"
  | "<" -> "lt"
  | "<=" -> "le"
  | ">" -> "gt"
  | ">=" -> "ge"
  | "set!" -> "set"
  | "string->symbol" -> "string_to_symbol"
  (* Predicates: convert ? suffix to is_ prefix *)
  | _ when String.length name > 0 && name.[String.length name - 1] = '?' ->
      "is_" ^ String.sub name 0 (String.length name - 1)
  (* Everything else: use name_sanitizer but strip leading underscore *)
  | _ ->
      let sanitized = Name_sanitizer.sanitize_var_name name in
      if String.length sanitized > 0 && sanitized.[0] = '_' then
        String.sub sanitized 1 (String.length sanitized - 1)
      else
        sanitized
  in
  "__builtin_" ^ sanitized

(* Translation strategy:

   In C we have: global functions, global variable declarations, and main function.
   ALL VALUES ARE HEAP-ALLOCATED POINTERS (Value pointers).

   1. FuncDefs -> global C functions (return pointer, take pointer array)
   2. VarDefs -> global pointer declaration + initialization in main
   3. Top-level Exprs -> execute in main

   All expressions use gcc block expression extension. *)

let ksu2c: cc_top_expr list -> string =
  let global_funcs = ref [] in
  let global_decls = ref [] in
  let main_body = ref [] in

  (* Translate expression to C expression string *)
  let rec t_expr (e: cc_expr): string = match e with
  (* Literals - use constructors, actual structs on stack *)
  | CC_Lit (Ast.L_Bool b) -> "MakeBool(" ^ string_of_bool b ^ ")"
  | CC_Lit (Ast.L_Number n) -> "MakeInt(" ^ string_of_int n ^ ")"
  | CC_Lit (Ast.L_String s) -> "MakeString(\"" ^ String.escaped s ^ "\")"
  | CC_Lit (Ast.L_Symbol s) -> "MakeSymbol(\"" ^ String.escaped s ^ "\")"

  (* Variables *)
  | CC_Var v -> v

  (* Control flow *)
  | CC_If (c, y, n) ->
      "(is_true(" ^ t_expr c ^ ") ? " ^ t_expr y ^ " : " ^ t_expr n ^ ")"

  (* Closures *)
  | CC_MakeClosure (fn, env) ->
      "MakeClosure(" ^ fn ^ ", " ^ t_expr env ^ ")"

  | CC_MakeEnv vars ->
      let n = List.length vars in
      (* Add one extra slot for self-reference patching (dirty fix for recursion) *)
      if n = 0 then "MakeEnv(1, \"\", NULL)"
      else
        let args = List.map (fun (name, e) ->
          let val_expr = match e with
            | CC_Var v -> v  (* already a Value* pointer *)
            | CC_EnvRef (env, var) -> "EnvRef(" ^ env ^ ", \"" ^ var ^ "\")"
            | _ -> failwith "CC_MakeEnv: expected Var or EnvRef"
          in
          "\"" ^ name ^ "\", " ^ val_expr
        ) vars in
        (* First entry is reserved for self-reference *)
        "MakeEnv(" ^ string_of_int (n + 1) ^ ", \"\", NULL, " ^ String.concat ", " args ^ ")"

  | CC_EnvRef (env, var) ->
      "EnvRef(" ^ env ^ ", \"" ^ var ^ "\")"  (* returns Value* directly *)

  (* Application *)
  | CC_App (fn, args) ->
      let args_str = String.concat ", " (List.map t_expr args) in
      (match fn with
      (* | CC_Prim P_Set -> "not implemented" *)
      | CC_Prim p ->
          prim_to_c_name p ^ "(" ^ args_str ^ ")"
      | _ ->
          let fn_str = t_expr fn in
          let argc = string_of_int (List.length args) in
          let argv = if args_str = "" then "NULL" else "(Value*[]){" ^ args_str ^ "}" in
          "ApplyClosure(" ^ fn_str ^ ", " ^ argc ^ ", " ^ argv ^ ")")

  | CC_Prim _ -> failwith "bug: CC_Prim should be handled in CC_App"
  | CC_Callcc _ -> failwith "callcc not implemented"

  in

  (* Translate top-level expression *)
  let t_top (top: cc_top_expr): unit = match top with
  | CC_FuncDef (name, args, body) ->
      (* First arg is always $env *)
      let c_args = match args with
        | "$env" :: _ -> "EnvEntry* $env, int argc, Value** argv"
        | _ -> failwith "CC_FuncDef: expected $env as first arg"
      in
      let arg_bindings = match args with
        | "$env" :: rest ->
            List.mapi (fun i a -> "  Value* " ^ a ^ " = argv[" ^ string_of_int i ^ "];") rest
        | _ -> []
      in
      let func =
        "Thunk " ^ name ^ "(" ^ c_args ^ ") {\n" ^
        String.concat "\n" arg_bindings ^
        (if arg_bindings <> [] then "\n" else "") ^
        "  return " ^ t_expr body ^ ";\n}"
      in
      global_funcs := !global_funcs @ [func]

  | CC_VarDef (name, expr) ->
      global_decls := !global_decls @ ["Value* " ^ name ^ ";"];
      (* For closures, add self-reference patching *)
      (match expr with
      | CC_App (CC_Var "id", [ CC_MakeClosure _ ]) -> 
          main_body := !main_body @ [name ^ " = Trampoline(" ^ t_expr expr ^ ");"];
          main_body := !main_body @ [name ^ "->closure.env[0].val = " ^ name ^ ";"];
          main_body := !main_body @ [name ^ "->closure.env[0].name = \"" ^ name ^ "\";"]
      | CC_App _ ->
          main_body := !main_body @ [name ^ " = Trampoline(" ^ t_expr expr ^ ");"]
      | _ ->
          main_body := !main_body @ [name ^ " = " ^ t_expr expr ^ ";"])

  | CC_Expr e ->
      match e with 
      | CC_App _ -> main_body := !main_body @ ["Trampoline(" ^ t_expr e ^ ");"]
      | _ -> main_body := !main_body @ [t_expr e ^ ";"]
  in

  fun exprs ->
    List.iter t_top exprs;

    let header = "#include \"ksu_builtins.c\"\n\n" in
    let decls = String.concat "\n" !global_decls in
    let funcs = String.concat "\n\n" !global_funcs in
    let body = String.concat "\n  " !main_body in

    header ^ decls ^ "\n\n" ^ funcs ^ "\n\nint main() {\n  nil = MakeNil();\n  id = MakeClosure(__id_impl, NULL);\n  " ^ body ^ "\n}\n"

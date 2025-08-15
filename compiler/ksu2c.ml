open Compiler_lib
open Closures
open Ast

type c_top_expr = 
| C_FunDef of (var * var list * c_statement) 
| C_VarDef of (var * c_expr)

and c_statement = 
| C_Return of c_expr 
| C_Assign of (var * c_expr)
| C_Invoke of (var * c_expr list)
| C_Block of c_statement list
| C_If of (c_expr * c_statement * c_statement)

and c_expr = 
| C_Int of int
| C_Bool of bool
| C_String of string
| C_Apply of (var * c_expr list)
| C_Ternary of (c_expr * c_expr * c_expr)
| C_Var of var


let ksu2c: Closures.cc_top_expr list -> c_top_expr list = 
  let gen_fresh_var domain =
    let num = ref 0 in 
    (fun () -> num := !num + 1; (domain ^ "_" ^ (string_of_int !num))) in
  let gen_main_expr = gen_fresh_var "MainExpr" in 

  let res = ref [] in 
  let append: c_top_expr -> unit = fun t -> res := t :: !res in 

  let main_body: c_statement list ref = ref [] in 
  let closure_envs: (var list) list ref = ref [] in

  let rec t_top: cc_top_expr -> unit = fun top_expr -> match top_expr with 
  | CC_FuncDef  (name, args, body) -> append (C_FunDef (name, args, C_Return (t body)))
  | CC_VarDef (name, e) -> append (C_VarDef (name, t e))
  | CC_Expr e -> main_body := C_Assign (gen_main_expr (), t e) :: !main_body

  and 
    t: cc_expr -> c_expr = function
    | CC_MakeClosure (func_id, env) -> failwith "lol"
    | CC_MakeEnv (vars) -> failwith "lol"
    | CC_EnvRef (env, var) -> failwith "lol"
    | CC_AppClosure (func, args) -> failwith "lol"
    (* And the regular stuff *)
    | CC_Bool (b) -> C_Bool b
    | CC_Number (n) -> C_Int n
    | CC_String (s) -> C_String s
    | CC_Var (v) -> C_Var v
    | CC_App (func, args) -> C_Apply (func, args)
    | CC_If (cond, y, n) -> C_If (cond, y, n)
    | CC_Callcc (e) -> failwith "lol"
    | CC_Let (defs, body) -> failwith "lol"
    | CC_Pair (e1, e2) -> failwith "lol"
    | CC_Nil -> failwith "lol"
    | CC_Prim (prim) -> failwith "lol"


in failwith "lol"
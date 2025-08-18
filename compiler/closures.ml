open Compiler_lib
open Ast


type cc_top_expr = 
| CC_FuncDef of var * var list * cc_expr
| CC_VarDef of var * cc_expr
| CC_EnvDef of var * var list
| CC_Expr of cc_expr

and cc_expr =
| CC_MakeClosure of var * cc_expr (* function id, environment - must actually be the Env*)
| CC_MakeEnv of var list * var (* env variables, env id (globalised function id)*)
| CC_EnvRef of cc_expr * var
| CC_AppClosure of cc_expr * cc_expr list
(* And the regular stuff *)
| CC_Bool of bool
| CC_Number of int
| CC_String of string
| CC_Var of var
| CC_If of cc_expr * cc_expr * cc_expr
| CC_Callcc of cc_expr
| CC_Let of (var * cc_expr) list * cc_expr
| CC_Pair of cc_expr * cc_expr
| CC_Nil
| CC_PrimApp of prim * cc_expr list

module VarSet = Set.Make(String)

(* Obtains set of free variables in an expression *)
let rec free: expr -> VarSet.t = fun expr -> 
  match expr.value with 
  (* Trivial Stuff *)
  | E_String _
  | E_Number _
  | E_Nil 
  | E_Bool _ -> VarSet.empty
  (* A bit less trivial *)
  | E_PrimApp (_, args) -> List.fold_left (fun free_vars expr -> VarSet.union free_vars (free expr)) VarSet.empty args
  | E_App (f, args) -> List.fold_left (fun free_vars expr -> VarSet.union free_vars (free expr)) VarSet.empty (f :: args)
  | E_Var v -> VarSet.singleton v
  | E_Callcc expr -> free expr
  | E_Pair (fst, snd) -> VarSet.union (free fst) (free snd)
  | E_If (c, y, n) -> VarSet.union (free n) @@ VarSet.union (free c) (free y)
  (* Lambda and Let *)
  | E_Let (defs, body) -> VarSet.diff (free body) (List.map fst defs |> VarSet.of_list)
  | E_Lambda (args, body) -> VarSet.diff (free body) (VarSet.of_list args)


(* Converts expression to the closure-converted. As all functions become global at this step, we also return list of FuncDefs *)
let convert: top_expr list -> cc_top_expr list = 
  let gen_fresh_var domain =
    let num = ref 0 in 
    (fun () -> num := !num + 1; (domain ^ "_" ^ (string_of_int !num))) in
  let gen_lambda_id = gen_fresh_var "Lambda" in 
  let gen_env_id = gen_fresh_var "Env" in 

  let res = ref [] in 
  let append: cc_top_expr -> unit = fun t -> res := t :: !res in 

  (* Auxillary function, f *)
  let rec t: VarSet.t -> var -> expr -> cc_expr = fun sub env_sym expr -> 
    let t' = t sub env_sym in
    
    match expr.value with 
    (* Trivial Stuff *)
    | E_String s -> CC_String s
    | E_Number n -> CC_Number n
    | E_PrimApp (p, args) -> CC_PrimApp (p, List.map t' args)
    | E_Nil -> CC_Nil
    | E_Bool b -> CC_Bool b
    (* A bit trickier *)
    | E_If (c, y, n) -> CC_If (t' c, t' y, t' n)
    | E_Let (defs, body) -> 
      let defs' = List.map (fun (v, expr) -> (v, t' expr)) defs in
      let body' = t (free body) env_sym body in 
      CC_Let (defs', body')
    | E_Pair (a, b) -> CC_Pair (t' a, t' b)
    | E_Callcc e -> CC_Callcc (t' e) 
    (* Var *)
    | E_Var v -> (match VarSet.find_opt v sub with Some v -> CC_EnvRef (CC_Var env_sym, v) | None -> CC_Var v)
    (* App & Lambda *)
    | E_App (fn, args) -> CC_AppClosure (t' fn, List.map t' args)
    | E_Lambda (args, body) -> 
      let lamid = gen_lambda_id () in 
      let envid = gen_env_id () in 
      let free_vars = free body in 
      let body' = t (free body) "$env" body in 
      append (CC_FuncDef (lamid, ("$env" :: args), body'));
      append (CC_EnvDef (envid, VarSet.to_list free_vars));
      CC_MakeClosure (lamid, CC_MakeEnv (VarSet.to_list free_vars, envid))
    in

  let t_top: top_expr -> unit = fun top_expr -> 
    let t = t VarSet.empty "Nonexistent env" in
    let transformed = match top_expr.value with 
      | E_Expr e -> CC_Expr (t e)
      | E_Define (name, e) -> CC_VarDef (name, t e)
  in append transformed
  

  in fun exprs -> List.iter t_top exprs; !res

  

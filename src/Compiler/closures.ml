open Lang
open Ast

type cc_top_expr =
  | CC_FuncDef of var * var list * cc_expr 
  | CC_VarDef of var * cc_expr
  | CC_Expr of cc_expr

and cc_expr =
  (* Closures *)
  | CC_MakeClosure of
      var * cc_expr (* function id, environment - (cc_expr must be the Env) *)
  | CC_MakeEnv of (var * cc_expr) list (* env variables*)
  | CC_EnvRef of var * var (* env id (i.e. $e_123) and id of the argument. *)
  | CC_App of cc_expr * cc_expr list (* May be application of closure or prim *)
  (* Literals *)
  | CC_Lit of Ast.lit
  (* Non-literals *)
  | CC_Var of var
  | CC_If of cc_expr * cc_expr * cc_expr
  | CC_Callcc of var * cc_expr
  | CC_Prim of prim

module VarSet = Set.Make (String)

(* Obtains set of free variables in an expression *)
let rec free : expr -> VarSet.t =
 fun expr ->
  match expr with
  (* Trivial Stuff *)
  | E_Lit _ | E_Prim _ -> VarSet.empty
  (* A bit less trivial *)
  | E_App (f, args) ->
      List.fold_left
        (fun free_vars expr -> VarSet.union free_vars (free expr))
        VarSet.empty (f :: args)
  | E_Var v -> VarSet.singleton v
  | E_Callcc (v, expr) -> VarSet.diff (free expr) (VarSet.singleton v)
  | E_If (c, y, n) -> VarSet.union (free n) @@ VarSet.union (free c) (free y)
  (* Lambda *)
  | E_Lambda (args, body) -> VarSet.diff (free body) (VarSet.of_list args)

(* Converts expression to the closure-converted. As all functions become global at this step, we also return list of FuncDefs *)
let convert : top_expr list -> cc_top_expr list =
  let gen_fresh_var domain =
    let num = ref 0 in
    fun () ->
      num := !num + 1;
      domain ^ "_" ^ string_of_int !num
  in
  let gen_lambda_id = gen_fresh_var "Lambda" in

  let res = ref [] in
  let append t = res := !res @ [ t ] in

  (* Auxillary function, checks if var is captured by closure  *)
  let cc_expr_of_var (sub : VarSet.t) (env_sym : var) (v : var) : cc_expr =
    if VarSet.mem v sub then CC_EnvRef (env_sym, v) else CC_Var v
  in

  let rec t : VarSet.t -> var -> expr -> cc_expr =
    (* Sub is set of variables that should be taken from the captured env via EnvRef *)
   fun sub env_sym expr ->
    let t' = t sub env_sym in

    match expr with
    (* Trivial Stuff *)
    | E_Lit lit -> CC_Lit lit
    | E_Prim p -> CC_Prim p
    (* A bit trickier *)
    | E_If (c, y, n) -> CC_If (t' c, t' y, t' n)
    | E_Callcc (v, e) -> CC_Callcc (v, t' e)
    (* Var *)
    | E_Var v -> cc_expr_of_var sub env_sym v
    (* App & Lambda *)
    | E_App (fn, args) -> CC_App (t' fn, List.map t' args)
    | E_Lambda (args, body) ->
        let lamid = gen_lambda_id () in
        let body' = t (free expr) "$env" body in
        let env =
          CC_MakeEnv
            (List.map
               (fun v -> (v, cc_expr_of_var sub env_sym v))
               (VarSet.to_list @@ free expr))
        in
        append (CC_FuncDef (lamid, "$env" :: args, body'));
        CC_MakeClosure (lamid, env)
  in

  (* FIXME 
    I'm not sure if this's correct - like assume that we 
    are inside the VarDef and reference previoously defined global def
    need to check that this is working as expected.  

    Ok, we don't pass global functions to the VarSet, even though we could. 
    
    What's going to happen now? actually it seems as it should work. If we call global functions
    on the top level - it's not in the sub, works alright. If we call some lambdas, then those functions 
    will be identified as free and passed to the env.
  *)
  let t_top : top_expr -> unit =
   fun top_expr ->
    let t = t VarSet.empty "If this env is called, it's a bug" in
    let transformed =
      match top_expr with
      | E_Expr e -> CC_Expr (t e)
      | E_Define (name, e) -> CC_VarDef (name, t e)
    in
    append transformed
  in
  fun exprs ->
    List.iter t_top exprs;
    !res

(* Pretty printing for closure-converted AST *)
let rec string_of_cc_expr = function
  | CC_Lit (Ast.L_Bool b) -> string_of_bool b
  | CC_Lit (Ast.L_Number n) -> string_of_int n
  | CC_Lit (Ast.L_String s) -> "\"" ^ String.escaped s ^ "\""
  | CC_Var v -> v
  | CC_If (c, y, n) -> "(if " ^ string_of_cc_expr c ^ " " ^ string_of_cc_expr y ^ " " ^ string_of_cc_expr n ^ ")"
  | CC_App (fn, args) ->
      let args_str = String.concat " " (List.map string_of_cc_expr args) in
      "(" ^ string_of_cc_expr fn ^ " " ^ args_str ^ ")"
  | CC_MakeClosure (fn, env) -> "(closure " ^ fn ^ " " ^ string_of_cc_expr env ^ ")"
  | CC_MakeEnv vars ->
      let vars_str = String.concat " " (List.map (fun (name, e) -> "(" ^ name ^ " " ^ string_of_cc_expr e ^ ")") vars) in
      "(env " ^ vars_str ^ ")"
  | CC_EnvRef (env, var) -> "(env-ref " ^ env ^ " \"" ^ var ^ "\")"
  | CC_Prim p -> "<prim:" ^ Builtins.builtin_to_string p ^ ">"
  | CC_Callcc (v, e) -> "(callcc " ^ v ^ ". " ^ string_of_cc_expr e ^ ")"

let string_of_cc_top_expr = function
  | CC_FuncDef (name, args, body) ->
      "(func-def " ^ name ^ " (" ^ String.concat " " args ^ ") " ^ string_of_cc_expr body ^ ")"
  | CC_VarDef (name, expr) ->
      "(var-def " ^ name ^ " " ^ string_of_cc_expr expr ^ ")"
  | CC_Expr e ->
      "(expr " ^ string_of_cc_expr e ^ ")"

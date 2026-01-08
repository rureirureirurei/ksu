open Lang
open Ast

type cc_top_expr =
(* One may ask why do we need both VarDef and FuncDef?
  Can't we replace FuncDef with Vardef f = lambda ... ?
  Actually no. In the converted code, we won't have any lambdas.
  Only globally defined named functions. That's the FuncDef.
  If we are having some Variable defined to equal expr where
  there is some lambda, i.e. 
  (define id (lambda y y))
  then it will be translated to 
  
  (define lambda_69 y y)
  (define id lambda_69)
  There is a minor tricky part - what about functions 
  that are already globally defined, i.e. 
  (define (id y) y)? That's syntax sugar for lambda, 
  check parser.mly
*)
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
  | CC_Bool of bool
  | CC_Number of int
  | CC_String of string
  | CC_Nil
  | CC_Begin of cc_expr list
  (* Non-literals *)
  | CC_Var of var
  | CC_If of cc_expr * cc_expr * cc_expr
  | CC_Callcc of cc_expr
  | CC_Let of (var * cc_expr) list * cc_expr
  | CC_Pair of cc_expr * cc_expr
  | CC_Prim of prim

module VarSet = Set.Make (String)

(* Obtains set of free variables in an expression *)
let rec free : expr -> VarSet.t =
 fun expr ->
  match expr.value with
  (* Trivial Stuff *)
  | E_String _ | E_Number _ | E_Prim _ | E_Nil | E_Bool _ -> VarSet.empty
  (* A bit less trivial *)
  | E_App (f, args) ->
      List.fold_left
        (fun free_vars expr -> VarSet.union free_vars (free expr))
        VarSet.empty (f :: args)
  | E_Var v -> VarSet.singleton v
  | E_Callcc expr -> free expr
  | E_Pair (fst, snd) -> VarSet.union (free fst) (free snd)
  | E_If (c, y, n) -> VarSet.union (free n) @@ VarSet.union (free c) (free y)
  (* Lambda and Let *)
  | E_Let (defs, body) ->
      (* Sequential let: each binding can see variables bound before it *)
      let (free_defs, _) =
        List.fold_left
          (fun (acc_free, acc_bound) (v, e) ->
            let free_e = VarSet.diff (free e) acc_bound in
            (VarSet.union acc_free free_e, VarSet.add v acc_bound))
          (VarSet.empty, VarSet.empty) defs
      in
      let free_body =
        VarSet.diff (free body) (List.map fst defs |> VarSet.of_list)
      in
      VarSet.union free_body free_defs
  | E_Begin exprs ->
      List.fold_left (fun acc e -> VarSet.union acc (free e)) VarSet.empty exprs
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

    match expr.value with
    (* Trivial Stuff *)
    | E_String s -> CC_String s
    | E_Number n -> CC_Number n
    | E_Prim p -> CC_Prim p
    | E_Nil -> CC_Nil
    | E_Bool b -> CC_Bool b
    (* A bit trickier *)
    | E_If (c, y, n) -> CC_If (t' c, t' y, t' n)
    | E_Begin exprs -> CC_Begin (List.map t' exprs)
    | E_Let (defs, body) ->
        (* Sequential let: adjust substitution set for each binding *)
        let (defs', bound) =
          List.fold_left
            (fun (acc_defs, prev_bound) (v, expr) ->
              let expr' = t (VarSet.diff sub prev_bound) env_sym expr in
              (acc_defs @ [(v, expr')], VarSet.add v prev_bound))
            ([], VarSet.empty) defs
        in
        let body' = t (VarSet.diff sub bound) env_sym body in
        CC_Let (defs', body')
    | E_Pair (a, b) -> CC_Pair (t' a, t' b)
    | E_Callcc e -> CC_Callcc (t' e)
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
      match top_expr.value with
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
  | CC_Bool b -> string_of_bool b
  | CC_Number n -> string_of_int n
  | CC_String s -> "\"" ^ String.escaped s ^ "\""
  | CC_Nil -> "nil"
  | CC_Var v -> v
  | CC_If (c, y, n) -> "(if " ^ string_of_cc_expr c ^ " " ^ string_of_cc_expr y ^ " " ^ string_of_cc_expr n ^ ")"
  | CC_Let (defs, body) ->
      let defs_str = String.concat " " (List.map (fun (v, e) -> "(" ^ v ^ " " ^ string_of_cc_expr e ^ ")") defs) in
      "(let (" ^ defs_str ^ ") " ^ string_of_cc_expr body ^ ")"
  | CC_Pair (a, b) -> "(pair " ^ string_of_cc_expr a ^ " " ^ string_of_cc_expr b ^ ")"
  | CC_App (fn, args) ->
      let args_str = String.concat " " (List.map string_of_cc_expr args) in
      "(" ^ string_of_cc_expr fn ^ " " ^ args_str ^ ")"
  | CC_MakeClosure (fn, env) -> "(closure " ^ fn ^ " " ^ string_of_cc_expr env ^ ")"
  | CC_MakeEnv vars ->
      let vars_str = String.concat " " (List.map (fun (name, e) -> "(" ^ name ^ " " ^ string_of_cc_expr e ^ ")") vars) in
      "(env " ^ vars_str ^ ")"
  | CC_EnvRef (env, var) -> "(env-ref " ^ env ^ " \"" ^ var ^ "\")"
  | CC_Prim p -> "<prim:" ^ string_of_prim p ^ ">"
  | CC_Callcc e -> "(callcc " ^ string_of_cc_expr e ^ ")"
  | CC_Begin exprs -> "(begin " ^ String.concat " " (List.map string_of_cc_expr exprs) ^ ")"

let string_of_cc_top_expr = function
  | CC_FuncDef (name, args, body) ->
      "(func-def " ^ name ^ " (" ^ String.concat " " args ^ ") " ^ string_of_cc_expr body ^ ")"
  | CC_VarDef (name, expr) ->
      "(var-def " ^ name ^ " " ^ string_of_cc_expr expr ^ ")"
  | CC_Expr e ->
      "(expr " ^ string_of_cc_expr e ^ ")"

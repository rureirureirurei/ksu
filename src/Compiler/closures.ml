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
  | CC_Bool of bool
  | CC_Number of int
  | CC_String of string
  | CC_Nil
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
      let free_defs =
        List.fold_left
          (fun acc (v, e) ->
            VarSet.union acc (VarSet.diff (free e) (VarSet.singleton v)))
          VarSet.empty defs
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
  let append : cc_top_expr -> unit = fun t -> res := !res @ [ t ] in

  (* Auxillary function, f *)
  let cc_expr_of_var (sub : VarSet.t) (env_sym : var) (v : var) : cc_expr =
    if VarSet.mem v sub then CC_EnvRef (env_sym, v) else CC_Var v
  in

  let rec t : VarSet.t -> var -> expr -> cc_expr =
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
    | E_Begin exprs -> (
        match List.rev exprs with
        | [] -> CC_Nil
        | last :: rev_rest ->
            let defs =
              List.rev rev_rest
              |> List.mapi (fun i e -> ("__seq_tmp_" ^ string_of_int i, t' e))
            in
            CC_Let (defs, t' last))
    | E_Let (defs, body) ->
        let defs' = List.map (fun (v, expr) -> (v, t' expr)) defs in
        (* Shadow bound variables inside the body by removing them from the env-captured set *)
        let bound = List.map fst defs |> VarSet.of_list in
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
        let free_vars = free expr in
        let body' = t (free expr) "$env" body in
        append (CC_FuncDef (lamid, "$env" :: args, body'));
        CC_MakeClosure
          ( lamid,
            CC_MakeEnv
              (List.map
                 (fun v -> (v, cc_expr_of_var sub env_sym v))
                 (VarSet.to_list free_vars)) )
  in

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

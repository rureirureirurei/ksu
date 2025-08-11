open Compiler_lib
open Ast
module VarSet = Set.Make (String)

(* Returns the free variables in the expression *)
let rec free : expr -> VarSet.t =
 fun expr ->
  match expr.value with
  | Lambda { ids; body } -> VarSet.diff (free body) (VarSet.of_list ids)
  | Var v -> VarSet.singleton v
  | App { func; args } ->
      List.fold_left
        (fun acc arg -> VarSet.union acc (free arg))
        (free func) args
  | Let { defs; body } ->
      let defs_vars =
        List.fold_left
          (fun acc (_, expr) -> VarSet.union acc (free expr))
          VarSet.empty defs
      in
      VarSet.diff (free body) defs_vars
  | Pair (e1, e2) -> VarSet.union (free e1) (free e2)
  | Nil -> VarSet.empty
  | If { cond; y; n } ->
      VarSet.union (free cond) (VarSet.union (free y) (free n))
  | Callcc _ -> failwith "Free variables analysis not implemented for callcc"
  | Bool _ | Number _ | String _ -> VarSet.empty
  | Prim _ -> VarSet.empty
  | Car e -> free e
  | Cdr e -> free e

(* Contains mappings identifier -> index in the enviroment for the free variables *)
module VarMap = Map.Make (String)

type state = int VarMap.t

let rec t_top_expr (expr : top_expr) : top_expr * top_expr list =
  match expr.value with
  | Define { name; expr } ->
      (* Should we add the define name to the state? *)
      let expr', globals = t expr VarMap.empty (fst @@ fresh_var ()) in
      ( { value = Define { name; expr = expr' }; id = expr.id; loc = expr.loc },
        globals )
  | Expr expr ->
      let expr', globals = t expr VarMap.empty (fst @@ fresh_var ()) in
      ({ value = Expr expr'; id = expr.id; loc = expr.loc }, globals)

(* Returns a tuple of the transformed expression and the list of global lambda definitions that need to be added to the state *)
and t : expr -> state -> expr -> expr * top_expr list =
 fun expr state env ->
  match expr.value with
  | Var v -> (
      match VarMap.find_opt v state with
      | Some index ->
          let index_node = synthetic (Number index) in
          let list_ref_node = synthetic (Var "list-ref") in
          let fresh, fresh_id = fresh_var () in
          let app_node =
            synthetic
              (App
                 {
                   func = fresh;
                   args = [ list_ref_node; env; index_node ];
                 })
          in
          (synthetic
              (Let { defs = [ (fresh_id, synthetic (Car list_ref_node)) ]; body = app_node}), [])

      | None -> (expr, []))
  | Lambda { ids; body } ->
      let free = free expr in
      let env', env'id = fresh_var () in
      let ids' = env'id :: ids in
      let state' =
        List.fold_left
          (fun acc (id, index) -> VarMap.add id index acc)
          VarMap.empty
          (List.combine (VarSet.elements free)
             (List.init (VarSet.cardinal free) (fun i -> i + 1)))
      in
      let body', body_globals = t body state' env' in
      let lambda_node = synthetic (Lambda { ids = ids'; body = body' }) in
      let lambda_name, lambda_name_id = fresh_var () in
      let lambda_global =
        synthetic (Define { name = lambda_name_id; expr = lambda_node })
      in
      let env =
        List.map
          (fun id -> fst @@ t (synthetic (Var id)) state env)
          (VarSet.elements free)
      in
      ( synthetic (Pair (lambda_name, genlist env)),
        body_globals @ [ lambda_global ] )
  | Let { defs; body } ->
      let defs_globals, defs' =
        List.fold_left_map
          (fun globals (id, expr) ->
            let e', global = t expr state env in
            (global @ globals, (id, e')))
          [] defs
      in
      let body', body_globals =
        t body
          (List.fold_left (fun acc (id, _) -> VarMap.remove id acc) state defs')
          env
      in
      ( synthetic @@ Let { defs = defs'; body = body' },
        defs_globals @ body_globals )
  | Pair (e1, e2) ->
      let e1', g1 = t e1 state env in
      let e2', g2 = t e2 state env in
      (synthetic @@ Pair (e1', e2'), g1 @ g2)
  | If { cond; y; n } ->
      let cond', gc = t cond state env in
      let y', gy = t y state env in
      let n', gn = t n state env in
      (synthetic @@ If { cond = cond'; y = y'; n = n' }, gc @ gy @ gn)
  | Callcc f ->
      let f', global_defs = t f state env in
      (synthetic (Callcc f'), global_defs)
  | Bool _ | Number _ | String _ -> (expr, [])
  | Prim _ -> (expr, [])
  | Nil -> (expr, [])
  | Car e ->
      let e', global = t e state env in
      (synthetic @@ Car e', global)
  | Cdr e ->
      let e', global = t e state env in
      (synthetic @@ Cdr e', global)
  | App { func = f; args } -> (
      let (globals : top_expr list), args' =
        List.fold_left_map
          (fun globals arg ->
            let e', global = t arg state env in
            (global @ globals, e'))
          [] args
      in
      match f.value with
      | Prim _ -> (synthetic @@ App { func = f; args = args' }, globals)
      | _ ->
          (* I am not sure if we need it? I mean the idea is clear - to have applications to identifiers only. *)
          let f', f'_global = t f state env in
          let var, var_id = fresh_var () in
          let var', var_id' = fresh_var () in
          ( synthetic
            @@ Let
                 {
                   defs = [ (var_id, f') ];
                   body =
                     synthetic
                     @@ Let
                          {
                            defs = [ (var_id', synthetic (Car var)) ];
                            body = synthetic (App { func = var'; args = var :: args' });
                          };
                 },
            f'_global @ globals ))

(* Takes a list of top_exprs and returns a list of top_exprs with the closures converted *)
let t_file : top_expr list -> top_expr list =
 fun exprs ->
  let globals, exprs' =
    List.fold_left_map
      (fun globals arg ->
        let expr', globals' = t_top_expr arg in
        (globals @ globals', expr'))
      [] exprs
  in
  globals @ exprs'

open Compiler_lib
open Ast
module VarSet = Set.Make (String)

(* Returns the free variables in the expression *)
let rec free : expr -> VarSet.t =
 fun expr ->
  match expr.value with
  | Lambda { ids; body } -> VarSet.diff (free body) (VarSet.of_list ids)
  | Var v -> VarSet.singleton v
  | App args ->
      List.fold_left
        (fun acc arg -> VarSet.union acc (free arg))
        VarSet.empty args
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
  | Cons (e1, e2) -> VarSet.union (free e1) (free e2)

(* Contains mappings identifier -> index in the enviroment for the free variables *)
module VarMap = Map.Make (String)

type state = int VarMap.t

let rec t_top_expr (expr : top_expr) : top_expr =
  match expr.value with
  | Define { name; expr } ->
      (* Should we add the define name to the state? *)
      let expr' = t expr VarMap.empty (fst @@ fresh_var ()) in
      { value = Define { name; expr = expr' }; id = expr.id; loc = expr.loc }
  | Expr expr ->
      {
        value = Expr (t expr VarMap.empty (fst @@ fresh_var ()));
        id = expr.id;
        loc = expr.loc;
      }

and t : expr -> state -> expr -> expr =
 fun expr state env ->
  match expr.value with
  | Var v -> (
      match VarMap.find_opt v state with
      | Some index ->
          let index_node = synthetic (Number index) in
          let list_ref_node = synthetic (Var "list-ref") in
          let app_node = synthetic (App [ (synthetic (Car list_ref_node)); list_ref_node; env; index_node ]) in
          app_node
      | None -> expr)
  | Lambda { ids; body } ->
      let free = free expr in
      let env', env'id = fresh_var () in
      let ids' = env'id :: ids in
      let state' =
        List.fold_left
          (fun acc (id, index) -> VarMap.add id index acc)
          VarMap.empty
          (List.combine (VarSet.elements free)
             (List.init (VarSet.cardinal free) (fun i -> (i + 1))))
      in
      let body' = t body state' env' in
      let lambda_node = synthetic (Lambda { ids = ids'; body = body' }) in
      genlist
        (lambda_node
        :: List.map
             (fun id -> t (synthetic (Var id)) state env)
             (VarSet.elements free))
  | Let { defs; body } ->
      let defs' = List.map (fun (id, expr) -> (id, t expr state env)) defs in
      let body' =
        t body
          (List.fold_left (fun acc (id, _) -> VarMap.remove id acc) state defs')
          env
      in
      synthetic @@ Let { defs = defs'; body = body' }
  | Pair (e1, e2) ->
      let e1' = t e1 state env in
      let e2' = t e2 state env in
      synthetic @@ Pair (e1', e2')
  | Nil -> expr
  | If { cond; y; n } ->
      let cond' = t cond state env in
      let y' = t y state env in
      let n' = t n state env in
      synthetic @@ If { cond = cond'; y = y'; n = n' }
  | Callcc _ -> failwith "Closure translation not implemented for callcc"
  | Bool _ | Number _ | String _ -> expr
  | Prim _ -> expr
  | Car e -> 
      let e' = t e state env in
      synthetic @@ Car e'
  | Cdr e -> 
      let e' = t e state env in
      synthetic @@ Cdr e'
  | Cons (e1, e2) -> 
      let e1' = t e1 state env in
      let e2' = t e2 state env in
      synthetic @@ Cons (e1', e2')
  | App (f :: args) -> (
      let args' = List.map (fun arg -> t arg state env) args in
      match f.value with
      | Var _ ->
        let var, var_id = fresh_var () in
        synthetic (Let { defs = [ (var_id, synthetic (Car f)) ]; body = synthetic (App (var :: f :: args')) })
      | Prim _ -> 
        synthetic
        @@ App (f :: args')
      | _ -> 
        let f' = t f state env in
        let var, var_id = fresh_var () in
        let var', var_id' = fresh_var () in
        synthetic @@ Let { 
          defs = [ (var_id, f') ]; 
          body = synthetic @@ Let { 
            defs = [ (var_id', synthetic (Car var)) ]; 
            body = synthetic (App (var' :: var :: args')) 
          } 
        }
        
      )
  | App [] -> failwith "Empty application"

(* Takes a list of top_exprs and returns a list of top_exprs with the closures converted *)
let t_file : top_expr list -> top_expr list =
 fun exprs -> List.map t_top_expr exprs

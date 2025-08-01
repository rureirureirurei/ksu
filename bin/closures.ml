open Compiler_lib
open Ast

(* Removes all the let bindings from the expression. Do we need it though? *)
let rec lets2lambdas: Ast.expr -> Ast.expr = fun expr ->
    match expr.value with
    | Ast.Let { defs; body } ->
        let lambda = Ast.Lambda { ids = List.map fst defs; body = lets2lambdas body } in
        let lambda_node = { value = lambda; loc = expr.loc; id = expr.id } in
        let args = lambda_node :: List.map (fun (_, expr) -> lets2lambdas expr) defs in 
        { value = Ast.App args; loc = expr.loc; id = expr.id }
    | _ -> expr


module VarSet = Set.Make(String)

let rec free_vars: Ast.expr -> VarSet.t = fun expr ->
    match expr.value with
    | Ast.Lambda { ids; body } ->
        VarSet.diff (free_vars body) (VarSet.of_list ids)
    | Ast.Var v -> VarSet.singleton v
    | Ast.App args -> List.fold_left (fun acc arg -> VarSet.union acc (free_vars arg)) VarSet.empty args
    | Ast.Let { defs; body } ->
        let defs_vars = List.fold_left (fun acc (_, expr) -> VarSet.union acc (free_vars expr)) VarSet.empty defs in
        VarSet.diff (free_vars body) defs_vars
    | Ast.Pair (e1, e2) ->
        VarSet.union (free_vars e1) (free_vars e2)
    | Ast.Nil -> VarSet.empty
    | Ast.If { cond; y; n } ->
        VarSet.union (free_vars cond) (VarSet.union (free_vars y) (free_vars n))
    | Ast.Callcc body ->
        free_vars body
    | Ast.Bool _ | Ast.Number _ | Ast.String _ -> VarSet.empty

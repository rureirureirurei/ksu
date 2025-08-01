open Compiler_lib
open Ast

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
    | Ast.Callcc _ -> failwith "Closure conversion not implemented for callcc"
    | Ast.Bool _ | Ast.Number _ | Ast.String _ -> VarSet.empty


(* Contains mappings identifier -> index in the enviroment for the free variables *)
module VarMap = Map.Make(String)
type state = int VarMap.t

let rec t (expr: top_expr): expr = 
    let rec aux: expr -> state -> expr -> expr = fun expr state env ->
        match expr.value with
        | Ast.Var v -> (match VarMap.find_opt v state with
            | Some index -> 
                let index_node = synthetic (Number index) in
                let list_ref_node = synthetic (Var "list-ref") in
                let app_node = synthetic (App [list_ref_node; env; index_node]) in
                app_node
            | None -> expr)
        | Ast.Lambda { ids; body } -> (
                let free_vars = free_vars expr in
                let env' = fresh_var () in
                let env'id = (match env'.value with 
                    | Var v -> v
                    | _ -> failwith "new env should be symbol") in
                let ids' = env'id :: ids in
                let state' = List.fold_left (fun acc (id, index) -> VarMap.add id index acc) VarMap.empty (List.combine (VarSet.elements free_vars) (List.init (VarSet.cardinal free_vars) (fun _ -> 0))) in
                let body' = aux body state' env' in
                let lambda_node = synthetic (Lambda { ids = ids'; body = body' }) in
                genlist
                    (lambda_node :: List.map 
                        (fun id -> aux (synthetic (Var id)) state env) 
                        (VarSet.elements free_vars)
                    )
            )
        | Ast.Let { defs; body } ->
            let defs' = List.map (fun (id, expr) -> (id, aux expr state env)) defs in
            let body' = aux body (List.fold_left (fun acc (id, _) -> VarMap.remove id acc) state defs') env in
            synthetic @@ Ast.Let { defs = defs'; body = body' }
        | _ -> failwith "not implemented"
    in 
        match expr.value with
        | Ast.Define { name; expr } ->
            (* Should we add the define name to the state? *)
            aux expr VarMap.empty (fresh_var ())
        | Ast.Expr expr ->
            aux expr VarMap.empty (fresh_var ())

let rec t_file: top_expr list -> top_expr list = failwith "not implemented"
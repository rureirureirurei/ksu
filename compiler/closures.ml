open Compiler_lib
open Ast

module VarSet = Set.Make(String)

let rec free_vars: expr -> VarSet.t = fun expr ->
    match expr.value with
    | Lambda { ids; body } ->
        VarSet.diff (free_vars body) (VarSet.of_list ids)
    | Var v -> VarSet.singleton v
    | App args -> List.fold_left (fun acc arg -> VarSet.union acc (free_vars arg)) VarSet.empty args
    | Let { defs; body } ->
        let defs_vars = List.fold_left (fun acc (_, expr) -> VarSet.union acc (free_vars expr)) VarSet.empty defs in
        VarSet.diff (free_vars body) defs_vars 
    | Pair (e1, e2) ->
        VarSet.union (free_vars e1) (free_vars e2)
    | Nil -> VarSet.empty
    | If { cond; y; n } ->
        VarSet.union (free_vars cond) (VarSet.union (free_vars y) (free_vars n))
    | Callcc _ -> failwith "Closure conversion not implemented for callcc"
    | Bool _ | Number _ | String _ -> VarSet.empty


(* Contains mappings identifier -> index in the enviroment for the free variables *)
module VarMap = Map.Make(String)
type state = int VarMap.t

let t (expr: top_expr): top_expr = 
    let rec aux: expr -> state -> expr -> expr = fun expr state env ->
        match expr.value with
        | Var v -> (match VarMap.find_opt v state with
            | Some index -> 
                let index_node = synthetic (Number index) in
                let list_ref_node = synthetic (Var "list-ref") in
                let app_node = synthetic (App [list_ref_node; env; index_node]) in
                app_node
            | None -> expr)
        | Lambda { ids; body } -> (
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
        | Let { defs; body } ->
            let defs' = List.map (fun (id, expr) -> (id, aux expr state env)) defs in
            let body' = aux body (List.fold_left (fun acc (id, _) -> VarMap.remove id acc) state defs') env in
            synthetic @@ Let { defs = defs'; body = body' }
        | Pair (e1, e2) ->
            let e1' = aux e1 state env in
            let e2' = aux e2 state env in
            synthetic @@ Pair (e1', e2')
        | Nil -> expr
        | If { cond; y; n } ->
            let cond' = aux cond state env in
            let y' = aux y state env in
            let n' = aux n state env in
            synthetic @@ If { cond = cond'; y = y'; n = n' }
        | Callcc _ -> failwith "todo"
        | Bool _ | Number _ | String _ -> expr
        (* we should probably check if the functino is lambda - transalte it and replace with let ..., otherwise if that's cont - todo, if id - then ((car id) (cdr id) ...args)  *)
        | App args -> (
            let args' = List.map (fun arg -> aux arg state env) args in
            match args' with
            | f :: args' -> (
                match f.value with
                | Var _ -> synthetic @@ App args'
                | Pair ({value = Lambda { ids = _; _ }; _}, _) -> (
                    let var = fresh_var () in 
                    let varid = (match var.value with
                        | Var v -> v
                        | _ -> failwith "wtf") in
                    let app'= synthetic @@ App ([
                        synthetic @@ App [
                            synthetic @@ Var "car";
                            var
                        ];
                        synthetic @@ App [
                            synthetic @@ Var "cdr";
                            var
                        ];
                    ] @ args') in 
                    synthetic @@ Ast.Let {
                        defs = [(varid, f)];
                        body = app'
                    })
                | _ -> failwith "wtf"
            )
            | [] -> failwith "cannot have empty application"
        )
    in 
        match expr.value with
        | Define { name; expr } ->
            (* Should we add the define name to the state? *)
            let expr' = aux expr VarMap.empty (fresh_var ()) in
            { value= Define { name = name; expr = expr' }; id = expr.id; loc = expr.loc }
        | Expr expr ->
            { value= Expr (aux expr VarMap.empty (fresh_var ())); id = expr.id; loc = expr.loc }

let t_file: top_expr list -> top_expr list = fun exprs ->
    List.map t exprs

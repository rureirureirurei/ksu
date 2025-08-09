open Compiler_lib
open Ast
module Trans = Map.Make (String)

type defs = (var * expr) list

let flatten : expr -> expr =
  let rec t (e : expr) (trans : expr Trans.t) : expr =
    match e.value with
    | Var v -> ( match Trans.find_opt v trans with None -> e | Some v -> v)
    | Let { defs; body } ->
        let defs' =
          List.map
            (fun (_, e) ->
              let fresh, fresh_id = fresh_var () in
              let trans' = Trans.add fresh_id fresh trans in
              let e' = t e trans' in
              (fresh_id, e'))
            defs
        in
        let trans' =
          List.fold_left
            (fun trans ((v, _), (v', _)) ->
              Trans.add v (synthetic (Var v')) trans)
            trans (List.combine defs defs')
        in
        synthetic (Let { defs = defs'; body = t body trans' })
    | Lambda _ ->
        failwith
          "Did not expect lambda during flattening. All lambdas must be first \
           child of define."
    | App (f :: args) -> (
        match f.value with
        | Prim _ | Var _ ->
            synthetic (App (f :: List.map (fun e -> t e trans) args))
        | _ -> failwith "Expected application to identifier or builtin")
    | App [] -> failwith "Expected application to have at least one argument"
    | If { cond; y; n } ->
        let cond' = t cond trans in
        let y' = t y trans in
        let n' = t n trans in
        synthetic (If { cond = cond'; y = y'; n = n' })
    | Callcc e ->
        let e' = t e trans in
        synthetic (Callcc e')
    | Pair (e1, e2) ->
        let e1' = t e1 trans in
        let e2' = t e2 trans in
        synthetic (Pair (e1', e2'))
    | Car e ->
        let e' = t e trans in
        synthetic (Car e')
    | Cdr e ->
        let e' = t e trans in
        synthetic (Cdr e')
    | Nil -> e
    | Prim _ -> e
    | Bool _ | String _ | Number _ -> e
  in
  fun e -> t e Trans.empty

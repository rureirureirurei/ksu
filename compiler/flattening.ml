open Compiler_lib
open Ast
module Trans = Map.Make (String)

type defs = (var * expr) list


let disambiguate : expr -> expr =
  let rec t (e : expr) (trans : expr Trans.t) : expr =
    match e.value with
    | Var v -> ( match Trans.find_opt v trans with None -> e | Some v -> v)
    | Let { defs; body } ->
        let fresh_vars = List.map (fun (var, _) -> (var, fresh_var ()) ) defs in
        let trans' = List.fold_left (fun trans (v, freshv) -> Trans.add v (fst freshv) trans) trans fresh_vars in 
        let (defs': (var * expr) list) =
          List.mapi
            (fun i (_, e) ->
              let (e': expr) = t e trans' in
              let (v': var) =  (List.nth fresh_vars i) |> snd |> snd in 
              (v', e')
            )
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
            synthetic (App ((t f trans) :: List.map (fun e -> t e trans) args))
        | _ -> failwith ("Expected application to identifier or builtin, found " ^ (Ast.string_of_expr f)))
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



  let disambiguate_top_expr : top_expr -> top_expr = 
    fun e -> 
      match e.value with 
      | Define { name; expr } -> (
        match expr.value with 
        | Lambda { ids; body } -> 
          let body' = disambiguate body in 
          synthetic (Define { name; expr = synthetic (Lambda { ids; body = body' }) })
        | _ -> synthetic (Define { name; expr = disambiguate expr }))
      | Expr e -> 
        let e' = disambiguate e in 
        synthetic (Expr e')




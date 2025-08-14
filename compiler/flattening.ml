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
    | App { func = f; args } -> (
        match f.value with
        | Prim _ | Var _ ->
            synthetic (App { func = t f trans; args = List.map (fun e -> t e trans) args })
        | _ -> failwith ("Expected application to identifier or builtin, found " ^ (Ast.string_of_expr f)))
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


(* Takes expression, and returns let expression (possibly with no definitions) that
  does NOT contain any nested let expressions.
*)
let rec flatten : expr -> expr = fun expr -> match expr.value with 
 | Pair (a, b) ->
  let a' = flatten a in 
  let b' = flatten b in (
  match (a'.value, b'.value) with 
  | (Let { defs = adefs; body = abody }, Let {defs = bdefs; body = bbody }) -> 
    synthetic (Let {defs = adefs @ bdefs; body = synthetic (Pair (abody, bbody))})
  | _ -> failwith "Expected two Let's")
| App { func; args } -> (
  let func' = flatten func in
  let args' = List.map (fun e -> flatten e) args in 
  let defs =
    (match func'.value with Let { defs; _ } -> defs | _ -> failwith "Expected let")
    :: List.map (fun (e : expr) -> match e.value with Let { defs; _ } -> defs | _ -> failwith "Expected let") args' in 
  let bodies =
    (match func'.value with Let { body; _ } -> body | _ -> failwith "Expected let")
    :: List.map (fun (e : expr) -> match e.value with Let { body; _ } -> body | _ -> failwith "Expected let") args' in 
  let func_body = List.hd bodies in
  let arg_bodies = List.tl bodies in
  synthetic (Let { defs = List.concat defs; body = synthetic (App { func = func_body; args = arg_bodies }) })
)
| Let { defs; body } -> (
  let defs' = List.map (fun (v, e) -> (v, flatten e)) defs in 
 
  let defs'' = 
  List.fold_left
    (fun defs'' (v, e) -> match e.value with 
    | Let { defs=defz; body } -> defs'' @ (defz @ [(v, body)])
    | _ -> failwith "Expected let") 
    []
    defs' 
  in 
 
  let body' = flatten body in match body'.value with 
  | Let { defs = body_defs; body = body'' } -> 
    synthetic (Let { defs = defs'' @ body_defs; body = body'' })
  | _ -> failwith "Expected let with no definitions"
)
| If { cond; y; n } -> (
  let cond' = flatten cond in 
  let y' = flatten y in 
  let n' = flatten n in 
  match (cond'.value, y'.value, n'.value) with 
  | (Let { defs = cond_defs; body = cond_body }, 
     Let { defs = y_defs; body = y_body }, 
     Let { defs = n_defs; body = n_body }) -> 
    synthetic (Let { defs = cond_defs @ y_defs @ n_defs; body = synthetic (If { cond = cond_body; y = y_body; n = n_body }) })
  | _ -> failwith "Expected three lets during if flattening"
)
| Callcc e -> (
  let e' = flatten e in match e'.value with 
  | Let { defs = e_defs; body = e_body } -> synthetic (Let { defs = e_defs; body = synthetic (Callcc e_body) })
  | _ -> failwith "Expected let during callcc flattening"
)
| Lambda _ -> failwith "Did not expect lambda during flattening. All lambdas must be first child of define."
| Number _  
| String _ 
| Nil
| Bool _ -> synthetic (Let { defs = []; body = expr })
| Var _
| Prim _ -> synthetic (Let { defs = []; body = expr })
| Car e -> (
  let e' = flatten e in match e'.value with 
  | Let { defs = e_defs; body = e_body } -> synthetic (Let { defs = e_defs; body = synthetic (Car e_body) })
  | _ -> failwith "Expected let during car flattening"
)
| Cdr e -> (
  let e' = flatten e in match e'.value with 
  | Let { defs = e_defs; body = e_body } -> synthetic (Let { defs = e_defs; body = synthetic (Cdr e_body) })
  | _ -> failwith "Expected let during cdr flattening"
)

let flatten_top_expr : top_expr -> top_expr = 
  fun e -> 
    match e.value with 
    | Define { name; expr } -> (match expr.value with 
      | Lambda { ids; body } -> 
        let body' = flatten body in 
        synthetic (Define { name; expr = synthetic (Lambda { ids; body = body' }) })
      | _ -> synthetic (Define { name; expr = flatten expr }))
    (* In top level exprs, we can just flatten the expression *)
    | Expr e -> 
      let e' = flatten e in 
      synthetic (Expr e')
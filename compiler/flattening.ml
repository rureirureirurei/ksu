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
            (fun (v, e) ->
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
    | _ -> failwith "todo"
  in
  fun e -> t e Trans.empty

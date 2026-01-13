open Lang
open Ast

(* This module provides CPS conversion:
  function
*)

let gensym =
  let cnt = ref 0 in
  fun s ->
    cnt := !cnt + 1;
    s ^ string_of_int !cnt

type cps_top_expr = CPS_Expr of cps_cxpr | CPS_Define of var * cps_cxpr

(* Atomic values *)
and cps_axpr =
  | CPS_Lit of Ast.lit
  | CPS_Var of var
  | CPS_Lambda of var list * cps_cxpr
  | CPS_Prim of prim
  | CPS_Id (* Special function that should be called as base continuation *)

(* Complex values *)
and cps_cxpr =
  | CPS_App of cps_axpr * cps_axpr list
  | CPS_If of cps_axpr * cps_cxpr * cps_cxpr

let is_axpr = function E_Lit _ | E_Var _ | E_Lambda _ -> true | _ -> false

let rec m : expr -> cps_axpr = function
  | E_Lit lit -> CPS_Lit lit
  | E_Var v -> CPS_Var v
  | E_Lambda (ids, body) ->
      let k = gensym "k" in
      let body' = t body (CPS_Var k) in
      CPS_Lambda (ids @ [ k ], body')
  | _ -> failwith "m expects expr that is atomic"

(* Takes expression, it's continuation and translates it *)
and t : expr -> cps_axpr -> cps_cxpr =
 fun e k ->
  match e with
  | E_App (f, args) -> (
      let argsyms = ref [] in
      let fsym = gensym "f" in
      let finl = match f with E_Prim p -> CPS_Prim p | _ -> CPS_Var fsym in
      let rec aux = function
        | [] -> CPS_App (finl, !argsyms @ [ k ])
        | hd :: rest ->
            let argsym = gensym "arg" in
            argsyms := !argsyms @ [ CPS_Var argsym ];
            t hd (CPS_Lambda ([ argsym ], aux rest))
      in
      match f with
      | E_Prim _ -> aux args
      | _ -> t f (CPS_Lambda ([ fsym ], aux args)))
  | E_If (c, y, n) ->
      let csym = gensym "c" in
      t c (CPS_Lambda ([ csym ], CPS_If (CPS_Var csym, t y k, t n k)))
  | E_Callcc (k', e) -> CPS_App (CPS_Lambda ([ k' ], t e k), [ k ])
  | E_Lit _ | E_Var _ | E_Lambda _ -> CPS_App (k, [ m e ])
  | E_Prim _ -> failwith "not expected prim in cps"

let t_top : top_expr -> cps_top_expr =
 fun e ->
  match e with
  | E_Expr e -> CPS_Expr (t e CPS_Id)
  | E_Define (v, e) -> CPS_Define (v, t e CPS_Id)

(* Back-translation from CPS to AST *)
let rec from_cps_axpr : cps_axpr -> expr = function
  | CPS_Lit lit -> E_Lit lit
  | CPS_Var v -> E_Var v
  | CPS_Lambda (ids, body) -> E_Lambda (ids, from_cps_cxpr body)
  | CPS_Prim p -> E_Prim p
  | CPS_Id -> E_Var "id"

and from_cps_cxpr : cps_cxpr -> expr = function
  | CPS_App (f, args) -> E_App (from_cps_axpr f, List.map from_cps_axpr args)
  | CPS_If (c, y, n) ->
      E_If (from_cps_axpr c, from_cps_cxpr y, from_cps_cxpr n)

let from_cps_top : cps_top_expr -> top_expr = function
  | CPS_Expr e -> E_Expr (from_cps_cxpr e)
  | CPS_Define (v, e) -> E_Define (v, from_cps_cxpr e)

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

(* removes begin and let constructions *)
    let last lst = List.hd @@ List.rev lst
let rec collapse_begin: expr_data -> expr_data = fun e -> match e with
| E_Begin es ->
  let unused_syms = List.map (fun _ -> gensym "unused_begin") es in
  let defs = List.map (fun (e, sym) -> (sym, e)) (List.combine es unused_syms) in
  collapse_begin (E_Let (defs, E_Var (last unused_syms)))
| E_Let (defs, body) ->
    let syms = List.map fst defs in
    let exprs = List.map snd defs in
    E_App (E_Lambda (syms, body), exprs)
| _ -> e

type cps_top_expr = CPS_Expr of cps_cxpr | CPS_Define of var * cps_cxpr
(* Atomic values *)
and cps_axpr =
  | CPS_Bool of bool
  | CPS_Var of var
  | CPS_String of string
  | CPS_Number of int
  | CPS_Lambda of var list * cps_cxpr
  | CPS_Id (* Special function that should be called as base continuation *)

(* Complex values *)
and cps_cxpr =
  | CPS_App of cps_axpr * cps_axpr list
  | CPS_If of cps_axpr * cps_cxpr * cps_cxpr
  | CPS_SetThen of var * cps_axpr * cps_cxpr

let is_axpr = function
  | E_Bool _ | E_Number _ | E_String _ | E_Var _ | E_Lambda _ -> true
  | _ -> false

let rec m : expr_data -> cps_axpr = function
  | E_Bool b -> CPS_Bool b
  | E_Number n -> CPS_Number n
  | E_String s -> CPS_String s
  | E_Var v -> CPS_Var v
  | E_Lambda (ids, body) ->
      let k = gensym "k" in
      let body' = t body (CPS_Var k) in
      CPS_Lambda (ids @ [ k ], body')
  | _ -> failwith "m expects expr that is atomic"
(* Takes expression, it's continuation and translates it *)

and t : expr_data -> cps_axpr -> cps_cxpr =
 fun e k ->
  match is_axpr e with
  | true -> CPS_App (k, [ m e ])
  | false -> (
      match e with
      | E_App (f, args) ->
          let argsyms = ref [] in
          let fsym = gensym "f" in
          let rec aux = function
            | [] -> CPS_App (CPS_Var fsym, !argsyms @ [ k ])
            | hd :: rest ->
                let argsym = gensym "arg" in
                argsyms := !argsyms @ [ CPS_Var argsym ];
                t hd (CPS_Lambda ([ argsym ], aux rest))
          in
          t f
            (CPS_Lambda ([ fsym ], aux args))
      | E_If (c, y, n) ->
          let csym = gensym "c" in
          t c
            (CPS_Lambda
               ([ csym ], CPS_If (CPS_Var csym, t y k, t n k)))
      | E_Callcc (k', e) -> CPS_App ((CPS_Lambda ([k'], t e k)), [k])
      | _ -> failwith "todo")

let t_top : top_expr -> cps_top_expr =
 fun e ->
  match e with
  | E_Expr e -> CPS_Expr (t e CPS_Id)
  | E_Define (v, e) -> CPS_Define (v, t e CPS_Id)

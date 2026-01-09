open Lang
open Ast

(* This module provides CPS conversion: 
  function 
*)

type cps_top_expr = CPS_Expr of cps_cxpr | CPS_Define of var * cps_cxpr

(* Atomic values *)
and cps_axpr =
  | CPS_Bool of bool
  | CPS_Var of var
  | CPS_String of string
  | CPS_Number of int
  | CPS_Lambda of var list * cps_cxpr
  | CPS_Nil
  | CPS_Pair of cps_axpr * cps_axpr
  | CPS_Id (* Special function that should be called as base continuation *)

(* Complex values *)
and cps_cxpr =
  | CPS_App of cps_axpr * cps_axpr list
  | CPS_If of cps_axpr * cps_cxpr * cps_cxpr

let is_axpr = function
  | E_Bool _ | E_Number _ | E_String _ | E_Var _ | E_Lambda _ | E_Pair _ -> true
  | _ -> false

let gensym =
  let cnt = ref 0 in
  fun s ->
    cnt := !cnt + 1;
    s ^ string_of_int !cnt

let rec m : expr_data -> cps_axpr = function
  | E_Bool b -> CPS_Bool b
  | E_Number n -> CPS_Number n
  | E_String s -> CPS_String s
  | E_Var v -> CPS_Var v
  | E_Nil -> CPS_Nil
  (* | E_Pair (l, r) ->  *)
  | E_Lambda (ids, body) ->
      let k = gensym "k" in
      let body' = t body.value (CPS_Var k) in
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
          t f.value
            (CPS_Lambda ([ fsym ], aux (List.map (fun a -> a.value) args)))
      | E_If (c, y, n) ->
          let csym = gensym "c" in
          t c.value
            (CPS_Lambda
               ([ csym ], CPS_If (CPS_Var csym, t y.value k, t n.value k)))
      | E_Callcc (k', e) -> CPS_App ((CPS_Lambda ([k'], t e.value k)), [k])
      | _ -> failwith "todo")

let t_top : top_expr -> cps_top_expr =
 fun e ->
  match e.value with
  | E_Expr e -> CPS_Expr (t e.value CPS_Id)
  | E_Define (v, e) -> CPS_Define (v, t e.value CPS_Id)

open Ast

module Env = Map.Make(String)
type 'a env = 'a Env.t
type res = Num of int | Bool of bool | Lambda of (string * Ast.expr)

let rec eval (e: Ast.expr) (env: 'a env): res  =
  match e with
  | Bool_Literal b -> Bool b
  | Int_Literal n -> Num n
  | Lambda {id; e} -> Lambda (id, e)
  | If {cond; y; n} -> (match eval cond env with
    | Bool b -> if b then eval y env else eval n env
    | _ -> failwith "Condition must be a boolean")
  | Let {id; e} -> eval e (Env.add id (eval e env) env)
  | Op { op; lhs; rhs } ->
    let v1 = eval lhs env in
    let v2 = eval rhs env in
    match op with
    | Plus -> (match (v1, v2) with
      | (Num n1, Num n2) -> Num (n1 + n2)
      | _ -> failwith "Operands must be numbers")
    | Minus -> (match (v1, v2) with
      | (Num n1, Num n2) -> Num (n1 - n2)
      | _ -> failwith "Operands must be numbers")
    | Times -> (match (v1, v2) with
      | (Num n1, Num n2) -> Num (n1 * n2)
      | _ -> failwith "Operands must be numbers")
    | Div -> (match (v1, v2) with
      | (Num n1, Num n2) -> Num (n1 / n2)
      | _ -> failwith "Operands must be numbers")
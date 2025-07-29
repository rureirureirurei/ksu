open Ast

module Env = Map.Make(String)
type 'a env = 'a Env.t
type res = Num of int 
  | Bool of bool 
  | String of string 
  | Symbol of string 
  | List of res list
  | Lambda of (string * Ast.expr)

let rec eval (e: Ast.expr) (env: 'a env): res  =
  match e with
  | Bool b -> Bool b
  | Number n -> Num n
  | String s -> String s
  | Symbol s -> Symbol s
  | List l -> List (List.map (fun e -> eval e env) l)
  | _ -> failwith "Not implemented"
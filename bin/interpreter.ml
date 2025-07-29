(* Interpreter module for KSU language *)
open Compiler_lib

let print_all_tokens lexbuf =
  let rec aux () =
    match Lexer.lex lexbuf with
    | EOF -> ()
    | _ as token ->
        print_endline (Lexer.string_of_token token);
        aux ()
  in
  aux ()

module Env = Map.Make (String)

type env = value Env.t
and value =
  | VNumber of int
  | VBool of bool
  | VString of string
  | VClosure of { args : string list; body: Ast.expr; env : env }

let string_of_value = function
  | VNumber n -> string_of_int n
  | VBool b -> string_of_bool b
  | VString s -> s
  | VClosure _ -> "<function>"

  
let rec eval (expr : Ast.expr) (env : env) : value =
  match expr with
  | Ast.Number n -> VNumber n
  | Ast.Bool b -> VBool b
  | Ast.String s -> VString s
  | Ast.Symbol sym ->  (
    match Env.find_opt sym env with
    | Some res -> res
    | None -> failwith @@ "Unknown symbol: " ^ sym
  )
  | Ast.Lambda { ids; body } ->
      VClosure { args = ids; body; env }
  | Ast.App exps -> (
      let rests = List.map (fun exp -> eval exp env) exps in
      let fun_res = List.hd rests in
      let args_val = List.tl rests in
      match fun_res with
      | VClosure { args; body; env } ->
          eval body (List.fold_left2 (fun acc id arg -> Env.add id arg acc) env args args_val)
      | _ -> failwith "Expected a function")
  | Ast.Callcc _ -> failwith "now implemented"
  | Ast.Let { ids; defs; b } ->
      let new_env =
        List.fold_left2
          (fun acc id def -> Env.add id (eval def env) acc)
          env ids defs
      in
      eval b new_env
  | Ast.If { cond; y; n } -> (
      match eval cond env with
      | VBool true -> eval y env
      | VBool false -> eval n env
      | _ -> failwith "if condition must be a boolean")
  | Ast.Define _ -> failwith "define can be only on top level"

let eval_file (exprs : Ast.expr list) (env : env) : value list =
  fst
  @@ List.fold_left
       (fun (rests, env) expr ->
         print_endline @@ Ast.string_of_expr expr;
         match expr with
         | Ast.Define { name; expr } ->
             let res = eval expr env in
             (rests, Env.add name res env)
         | _ as expr ->
             let res = eval expr env in
             (res :: rests, env))
       ([], env) exprs

let interpret files =
  Printf.printf "Interpreting files: %s\n" (String.concat ", " files);
  let channel = open_in (List.hd files) in
  let lexbuf = Lexing.from_channel channel in
  let parse_tree = Parser.parse Lexer.lex lexbuf in
  let res = eval_file parse_tree Env.empty in
  List.iter (fun r -> print_endline @@ string_of_value r) res

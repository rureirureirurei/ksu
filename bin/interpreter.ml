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

type res =
  | Num of int
  | Bool of bool
  | String of string
  | Fun of (res list -> res)

type env = res Env.t

let string_of_res = function
  | Num n -> string_of_int n
  | Bool b -> string_of_bool b
  | String s -> s
  | Fun _ -> "<function>"

let rec eval (expr : Ast.expr) (env : env) : res =
  match expr with
  | Ast.Number n -> Num n
  | Ast.Bool b -> Bool b
  | Ast.String s -> String s
  | Ast.Symbol sym -> (
      match sym with
      | "-"
      | "+"
      | "*"
      | "/" ->
          let op_of_sym = function
            | "-" -> (-)
            | "+" -> (+)
            | "*" -> ( * )
            | "/" -> (/)
            | _ -> failwith "Unknown symbol"
          in
          Fun
            (fun args ->
              match args with
              | [ Num n1; Num n2 ] -> Num ((op_of_sym sym) n1 n2)
              | _ -> failwith "op expects 2 nums")
      | "="
      | "<"
      | ">"
      | "<="
      | ">=" ->
          let op_of_sym = function
            | "=" -> (=)
            | "<" -> (<)
            | ">" -> (>)
            | "<=" -> (<=)
            | ">=" -> (>=)
            | _ -> failwith "Unknown symbol"
          in
          Fun
            (fun args ->
              match args with
              | [ Num n1; Num n2 ] -> Bool ((op_of_sym sym) n1 n2)
              | _ -> failwith "op expects 2 nums")
      | _ -> (
          match Env.find_opt sym env with
          | Some res -> res
          | None -> failwith @@ "Unknown symbol: " ^ sym))
  | Ast.Lambda { ids; body } ->
      Fun
        (fun args ->
          let computed_args = List.combine ids args in
          let new_env =
            List.fold_left
              (fun acc (id, arg) -> Env.add id arg acc)
              env computed_args
          in
          eval body new_env)
  | Ast.App exps -> (
      let rests = List.map (fun exp -> eval exp env) exps in
      let fun_res = List.hd rests in
      let args = List.tl rests in
      match fun_res with Fun f -> f args | _ -> failwith "Expected a function")
  | Ast.Callcc _ -> failwith "now implemented"
  | Ast.Let { ids; defs; b } -> (
      let new_env =
        List.fold_left2
          (fun acc id def -> Env.add id (eval def env) acc)
          env ids defs
      in
      eval b new_env)
  | Ast.If { cond; y; n } -> (
      match eval cond env with
      | Bool true -> eval y env
      | Bool false -> eval n env
      | _ -> failwith "if condition must be a boolean")
  | Ast.Define _ -> failwith "define can be only on top level"

let eval_file (exprs : Ast.expr list) (env : env) : res list =
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
  List.iter (fun r -> print_endline @@ string_of_res r) res

(* Interpreter module for KSU language *)
open Compiler_lib

(* Utility function for printing all tokens *)
let print_all_tokens lexbuf =
  let rec aux () =
    match Lexer.lex lexbuf with
    | EOF -> ()
    | token ->
        print_endline (Lexer.string_of_token token);
        aux ()
  in
  aux ()

(* Environment implementation *)
module Env = Map.Make (String)

(* Value and environment type definitions *)
type env = value Env.t

and value =
  | VNumber of int
  | VBool of bool
  | VString of string
  | VClosure of { args : string list; body : Ast.expr; env : env }
  | VRecClosure of {
      name : string;
      args : string list;
      body : Ast.expr;
      env : env;
    }

(* Convert values to string representation *)
let string_of_value = function
  | VNumber n -> string_of_int n
  | VBool b -> string_of_bool b
  | VString s -> s
  | VClosure _ -> "<function>"
  | VRecClosure _ -> "<rec-function>"

(* Helper function to extract number from value *)
let value_to_number = function
  | VNumber n -> n
  | _ -> failwith "Expected a number"

(* Helper function to extract boolean from value *)
let value_to_bool = function VBool b -> b | _ -> failwith "Expected a boolean"

(* Handle built-in operations *)
let rec eval_builtin_op op args env =
  match op with
  | "*" -> eval_multiplication args env
  | "+" -> eval_addition args env
  | "=" -> eval_equality args env
  | "-" -> eval_subtraction args env
  | _ -> failwith ("Unknown built-in operation: " ^ op)

(* Arithmetic and comparison operations *)
and eval_multiplication args env =
  let values = List.map (eval_expr env) args in
  let numbers = List.map value_to_number values in
  let product = List.fold_left ( * ) 1 numbers in
  VNumber product

and eval_addition args env =
  let values = List.map (eval_expr env) args in
  let numbers = List.map value_to_number values in
  let sum = List.fold_left ( + ) 0 numbers in
  VNumber sum

and eval_equality args env =
  let values = List.map (eval_expr env) args in
  let numbers = List.map value_to_number values in
  let is_equal =
    List.length numbers = 2 && List.hd numbers = List.hd (List.tl numbers)
  in
  VBool is_equal

and eval_subtraction args env =
  let values = List.map (eval_expr env) args in
  let numbers = List.map value_to_number values in
  let difference =
    match numbers with
    | [] -> 0
    | [ x ] -> -x
    | x :: xs -> List.fold_left ( - ) x xs
  in
  VNumber difference

(* Main expression evaluation function *)
and eval_expr env = function
  | Ast.Number n -> VNumber n
  | Ast.Bool b -> VBool b
  | Ast.String s -> VString s
  | Ast.Symbol sym -> (
      match Env.find_opt sym env with
      | Some value -> value
      | None -> failwith ("Unknown symbol: " ^ sym))
  | Ast.Lambda { ids; body } -> VClosure { args = ids; body; env }
  | Ast.App (Ast.Symbol op :: args) when List.mem op [ "*"; "+"; "="; "-" ] ->
      eval_builtin_op op args env
  | Ast.App (func_expr :: arg_exprs) -> (
      let func_value = eval_expr env func_expr in
      let arg_values = List.map (eval_expr env) arg_exprs in
      match func_value with
      | VClosure { args; body; env } ->
          let new_env =
            List.fold_left2
              (fun acc id arg -> Env.add id arg acc)
              env args arg_values
          in
          eval_expr new_env body
      | VRecClosure { name; args; body; env } as closure ->
          let new_env =
            List.fold_left2
              (fun acc id arg -> Env.add id arg acc)
              env args arg_values
          in
          let final_env = Env.add name closure new_env in
          eval_expr final_env body
      | _ -> failwith "Expected a function")
  | Ast.App [] -> failwith "Empty application"
  | Ast.Callcc _ -> failwith "Callcc not implemented"
  | Ast.Let { ids; defs; b } ->
      let new_env =
        List.fold_left2
          (fun acc id def -> Env.add id (eval_expr env def) acc)
          env ids defs
      in
      eval_expr new_env b
  | Ast.If { cond; y; n } ->
      let condition_value = eval_expr env cond in
      if value_to_bool condition_value then eval_expr env y else eval_expr env n
  | Ast.Define _ -> failwith "Define can only be used at top level"

(* Handle top-level definitions *)
let process_definition name expr env =
  match expr with
  | Ast.Lambda { ids; body } ->
      let recursive_closure = VRecClosure { name; args = ids; body; env } in
      Env.add name recursive_closure env
  | _ ->
      let value = eval_expr env expr in
      Env.add name value env

(* Evaluate a list of top-level expressions *)
let eval_file exprs initial_env =
  let results, _ =
    List.fold_left
      (fun (results_acc, current_env) expr ->
        match expr with
        | Ast.Define { name; expr } ->
            let updated_env = process_definition name expr current_env in
            (results_acc, updated_env)
        | _ ->
            let result = eval_expr current_env expr in
            (result :: results_acc, current_env))
      ([], initial_env) exprs
  in
  List.rev results

(* Main interpreter entry point *)
let interpret files =
  let filename = List.hd files in
  let channel = open_in filename in
  let lexbuf = Lexing.from_channel channel in
  let parse_tree = Parser.parse Lexer.lex lexbuf in
  let results = eval_file parse_tree Env.empty in
  List.iter (fun result -> print_endline (string_of_value result)) results

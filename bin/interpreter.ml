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
let rec value_to_bool = function
  | VBool b -> b
  | _ -> failwith "Expected a boolean"

(* Handle built-in operations *)
and eval_builtin_op op args env k =
  eval_exprs env args [] (fun values ->
      let numbers = List.map value_to_number values in
      k
      @@
      match op with
      | "+" -> VNumber (List.fold_left ( + ) 0 numbers)
      | "=" ->
          VBool
            (List.length numbers = 2
            && List.hd numbers = List.hd (List.tl numbers))
      | "-" ->
          VNumber (List.fold_left ( - ) (List.hd numbers) (List.tl numbers))
      | "*" -> VNumber (List.fold_left ( * ) 1 numbers)
      | _ -> failwith ("Unknown operation: " ^ op))

(* Evalualte a list of expressions sequentially, expressions cannot reference variables in each other similar to regular let*)
and eval_exprs :
    'a. env -> Ast.expr list -> value list -> (value list -> 'a) -> 'a =
 fun env args acc k ->
  match args with
  | [] -> k acc
  | a :: args -> eval_expr env a (fun v -> eval_exprs env args (acc @ [ v ]) k)

and extend_env : env -> (string * value) list -> env =
  List.fold_left (fun acc (name, value) -> Env.add name value acc)

(* Main expression evaluation function *)
and eval_expr : 'a. env -> Ast.expr -> (value -> 'a) -> 'a =
 fun env expr k ->
  match expr with
  | Ast.Number n -> k (VNumber n)
  | Ast.Bool b -> k (VBool b)
  | Ast.String s -> k (VString s)
  | Ast.Symbol sym -> (
      match Env.find_opt sym env with
      | Some value -> k value
      | None -> failwith ("Unknown symbol: " ^ sym))
  | Ast.Lambda { ids; body } -> k (VClosure { args = ids; body; env })
  | Ast.App (Ast.Symbol op :: args) when List.mem op [ "*"; "+"; "="; "-" ] ->
      eval_builtin_op op args env k
  | Ast.App (func_expr :: arg_exprs) ->
      eval_expr env func_expr (fun func_value ->
          match func_value with
          | VClosure { args; body; env } ->
              eval_exprs env arg_exprs [] (fun arg_values ->
                  let extended_env =
                    extend_env env (List.combine args arg_values)
                  in
                  eval_expr extended_env body k)
          | VRecClosure { name; args; body; env } ->
              eval_exprs env arg_exprs [] (fun arg_values ->
                  let extended_env =
                    Env.add name func_value
                      (extend_env env (List.combine args arg_values))
                  in
                  eval_expr extended_env body k)
          | _ -> failwith "Expected a function")
  | Ast.App [] -> failwith "Empty application"
  | Ast.Callcc _ -> failwith "Callcc not implemented"
  | Ast.Let { defs; body } ->
      eval_exprs env (List.map snd defs) [] (fun values ->
          let extended_env =
            extend_env env (List.combine (List.map fst defs) values)
          in
          eval_expr extended_env body k)
  | Ast.If { cond; y; n } ->
      eval_expr env cond (fun condition_value ->
          if value_to_bool condition_value then eval_expr env y k
          else eval_expr env n k)
  | Ast.Define _ -> failwith "Define can only be used at top level"

(* Handle top-level definitions *)
let process_definition : string -> Ast.expr -> env -> env =
 fun name expr env ->
  match expr with
  | Ast.Lambda { ids; body } ->
      let recursive_closure = VRecClosure { name; args = ids; body; env } in
      Env.add name recursive_closure env
  | _ -> eval_expr env expr (fun v -> Env.add name v env)

(* Evaluate a list of top-level expressions *)
let eval_file : Ast.expr list -> env -> value list =
 fun exprs initial_env ->
  let results, _ =
    List.fold_left
      (fun (acc, env) expr ->
        match expr with
        | Ast.Define { name; expr } ->
            let updated_env = process_definition name expr env in
            (acc, updated_env)
        | _ -> eval_expr env expr (fun v -> (v :: acc, env)))
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

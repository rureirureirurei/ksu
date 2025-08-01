(* Interpreter module for KSU language *)
open Compiler_lib

(* Error handling utilities *)
let format_error_position lexbuf =
  let pos = Lexing.lexeme_start_p lexbuf in
  Printf.sprintf "at line %d, column %d" pos.Lexing.pos_lnum (pos.Lexing.pos_cnum - pos.Lexing.pos_bol)

let format_parser_error lexbuf =
  let token = Lexing.lexeme lexbuf in
  let position = format_error_position lexbuf in
  if String.length token > 0 then
    Printf.sprintf "syntax error: unexpected token '%s' %s" token position
  else
    Printf.sprintf "syntax error: unexpected end of input %s" position

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
  | VCont of (value -> value)
  | VPair of value * value
  | VNil
  | VPrim of (value list -> value)

(* Convert values to string representation *)
let rec string_of_value = function
  | VNumber n -> string_of_int n
  | VBool b -> string_of_bool b
  | VString s -> s
  | VClosure _ -> "<function>"
  | VRecClosure _ -> "<rec-function>"
  | VCont _ -> "<continuation>"
  | VPair (a, b) -> "(" ^ string_of_value a ^ " . " ^ string_of_value b ^ ")"
  | VNil -> "nil"
  | VPrim _ -> "<builtin>"

let print_env env =
  print_endline "Environment:";
  Env.iter (fun k v -> print_endline (k ^ " -> " ^ string_of_value v)) env

(* Helper function to extract number from value *)
let value_to_number = function
  | VNumber n -> n
  | _ -> failwith "Expected a number"

(* Helper function to extract boolean from value *)
let value_to_bool = function VBool b -> b | _ -> failwith "Expected a boolean"

let init_env =
  let aux f = VPrim (fun args -> f (List.map value_to_number args)) in
  Env.of_list
    [
      ("+", aux (fun args -> VNumber (List.fold_left ( + ) 0 args)));
      ("*", aux (fun args -> VNumber (List.fold_left ( * ) 1 args)));
      ( "-",
        aux (fun args ->
            VNumber (List.fold_left ( - ) (List.hd args) (List.tl args))) );
      ( "/",
        aux (fun args ->
            VNumber (List.fold_left ( / ) (List.hd args) (List.tl args))) );
      ( "=",
        aux (fun args -> VBool (List.for_all (fun x -> x = List.hd args) args))
      );
      ( "<",
        aux (function
          | [ x; y ] -> VBool (x < y)
          | _ -> failwith "Expected two arguments for <") );
      ( ">",
        aux (function
          | [ x; y ] -> VBool (x > y)
          | _ -> failwith "Expected two arguments for >") );
      ( "<=",
        aux (function
          | [ x; y ] -> VBool (x <= y)
          | _ -> failwith "Expected two arguments for <=") );
      ( ">=",
        aux (function
          | [ x; y ] -> VBool (x >= y)
          | _ -> failwith "Expected two arguments for >=") );
      ( "cons",
        VPrim
          (function
          | [ x; y ] -> VPair (x, y)
          | _ -> failwith "Expected two arguments for cons") );
      ( "car",
        VPrim
          (function
          | [ VPair (x, _) ] -> x
          | _ -> failwith "Expected a pair for car") );
      ( "cdr",
        VPrim
          (function
          | [ VPair (_, y) ] -> y
          | _ -> failwith "Expected a pair for cdr") );
      ("nil", VNil);
      ("null?", VPrim (function [ VNil ] -> VBool true | _ -> VBool false));
      ("pair?", VPrim (function [ VPair _ ] -> VBool true | _ -> VBool false));
      ( "list?",
        VPrim
          (function
          | [ VNil ] -> VBool true
          | [ VPair (_, VNil) ] -> VBool true
          | _ -> VBool false) );
      ( "eq?",
        VPrim
          (function
          | [ x; y ] -> VBool (x = y)
          | _ -> failwith "Expected two arguments for eq?") );
    ]

(* Evalualte a list of expressions sequentially, expressions cannot reference variables in each other similar to regular let*)
let rec eval_exprs :
    env -> Ast.expr list -> value list -> (value list -> value) -> value =
 fun env args acc k ->
  match args with
  | [] -> k acc
  | a :: args -> eval_expr env a (fun v -> eval_exprs env args (acc @ [ v ]) k)

and extend_env : env -> (string * value) list -> env =
  List.fold_left (fun acc (name, value) -> Env.add name value acc)

(* Main expression evaluation function *)
and eval_expr : env -> Ast.expr -> (value -> value) -> value =
 fun env { value; _ } k ->
  match value with
  | Ast.Number n -> k (VNumber n)
  | Ast.Bool b -> k (VBool b)
  | Ast.String s -> k (VString s)
  | Ast.Var sym -> (
      match Env.find_opt sym env with
      | Some value -> k value
      | None -> failwith ("Unknown symbol: " ^ sym))
  | Ast.Lambda { ids; body } -> k (VClosure { args = ids; body; env })
  | Ast.App (func_expr :: arg_exprs) ->
      eval_expr env func_expr (fun func_value ->
          match func_value with
          | VClosure { args; body; env = captured_env } ->
              eval_exprs env arg_exprs [] (fun arg_values ->
                  if List.length arg_values <> List.length args then
                    failwith (Printf.sprintf "Expected %d arguments, got %d" 
                               (List.length args) (List.length arg_values))
                  else
                    let extended_env =
                      extend_env captured_env (List.combine args arg_values)
                    in
                    eval_expr extended_env body k)
          | VRecClosure { name; args; body; env = captured_env } ->
              eval_exprs env arg_exprs [] (fun arg_values ->
                  if List.length arg_values <> List.length args then
                    failwith (Printf.sprintf "Expected %d arguments, got %d" 
                               (List.length args) (List.length arg_values))
                  else
                    let extended_env =
                      Env.add name func_value
                        (extend_env captured_env (List.combine args arg_values))
                    in
                    eval_expr extended_env body k)
          | VCont f ->
              eval_exprs env arg_exprs [] (fun arg_values ->
                  f (List.hd arg_values))
          | VPrim f ->
              eval_exprs env arg_exprs [] (fun arg_values -> k (f arg_values))
          | _ -> failwith "Expected a function")
  | Ast.App [] -> failwith "Empty application"
  | Ast.Callcc { value = Ast.Lambda { ids; body }; _ } -> (
      match ids with
      | [ id ] ->
          let extended_env = Env.add id (VCont k) env in
          eval_expr extended_env body k
          (* We may pass the (_ -> "failwith call/cc must end with k call" ) *)
      | _ -> failwith "Callcc expected a lambda with one argument")
  | Ast.Callcc _ -> failwith "Callcc expected a lambda"
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
  | Ast.Pair (a, b) ->
      eval_expr env a (fun a_value ->
          eval_expr env b (fun b_value -> k (VPair (a_value, b_value))))
  | Ast.Nil -> k VNil

(* Handle top-level definitions *)
let process_definition : string -> Ast.expr -> env -> env =
 fun name expr env ->
  match expr with
  | { value = Ast.Lambda { ids; body }; _ } ->
      let recursive_closure = VRecClosure { name; args = ids; body; env } in
      Env.add name recursive_closure env
  | _ ->
      let v = eval_expr env expr (fun v -> v) in
      Env.add name v env

(* Evaluate a list of top-level expressions *)
let eval_file : Ast.top_expr list -> env -> value list =
 fun exprs env ->
  let results, _ =
    List.fold_left
      (fun (acc, env) (expr: Ast.top_expr) ->
        match expr.value with
        | Ast.Define { name; expr } ->
            let updated_env = process_definition name expr env in
            (acc, updated_env)
        | Ast.Expr expr ->
            let v = eval_expr env expr (fun v -> v) in
            (v :: acc, env))
      ([], env) exprs
  in
  List.rev results

(* Main interpreter entry point *)
let interpret files =
  let filename = List.hd files in
  let channel = open_in filename in
  let lexbuf = Lexing.from_channel channel in
  
  try
    let parse_tree = Parser.parse Lexer.lex lexbuf in
    let results = eval_file parse_tree init_env in
    List.iter (fun result -> print_endline (string_of_value result)) results
  with
  | Parser.Error ->
      let error_msg = format_parser_error lexbuf in
      Printf.eprintf "Parse error in %s: %s\n" filename error_msg;
      exit 1
  | Sys_error msg ->
      Printf.eprintf "Error opening file %s: %s\n" filename msg;
      exit 1
  | e ->
      Printf.eprintf "Unexpected error: %s\n" (Printexc.to_string e);
      exit 1

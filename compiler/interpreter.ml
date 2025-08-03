(* Interpreter module for KSU language *)
open Compiler_lib

(* Error handling utilities *)
let format_error_position lexbuf =
  let pos = Lexing.lexeme_start_p lexbuf in
  Printf.sprintf "at line %d, column %d" pos.Lexing.pos_lnum
    (pos.Lexing.pos_cnum - pos.Lexing.pos_bol)

let format_parser_error lexbuf =
  let token = Lexing.lexeme lexbuf in
  let position = format_error_position lexbuf in
  if String.length token > 0 then
    Printf.sprintf "syntax error: unexpected token '%s' %s" token position
  else Printf.sprintf "syntax error: unexpected end of input %s" position

(* Stack trace tracking *)
type eval_context = {
  expr : Ast.expr;
  description : string;
}

let eval_stack : eval_context list ref = ref []

let push_context expr description =
  eval_stack := { expr; description } :: !eval_stack

let pop_context () =
  match !eval_stack with
  | _ :: rest -> eval_stack := rest
  | [] -> ()

let format_stack_trace () =
  let rec aux stack acc =
    match stack with
    | [] -> acc
    | { expr; description } :: rest ->
        let loc = expr.loc in
        let location_str = 
          if loc.file = "" then "unknown location"
          else Printf.sprintf "%s:%d:%d" loc.file loc.line loc.column
        in
        let expr_str = Ast.string_of_expr expr in
        let line = Printf.sprintf "  %s: %s\n    %s" location_str description expr_str in
        aux rest (line :: acc)
  in
  let lines = aux !eval_stack [] in
  "Stack trace:\n" ^ String.concat "\n" (List.rev lines)

let runtime_error msg =
  let stack_trace = format_stack_trace () in
  Printf.eprintf "Runtime error: %s\n%s\n" msg stack_trace;
  exit 1

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
let rec string_of_value ?(offset = 0) = function
  | VNumber n -> string_of_int n
  | VBool b -> string_of_bool b
  | VString s -> s
  | VClosure { args; body; _ } -> 
      let indent = String.make offset ' ' in
      let body_str = Ast.string_of_expr body in
      "<function (" ^ String.concat " " args ^ ")\n" ^ indent ^ "  " ^ body_str ^ ">"
  | VRecClosure { name; args; body; _ } -> 
      let indent = String.make offset ' ' in
      let body_str = Ast.string_of_expr body in
      "<rec-function " ^ name ^ " (" ^ String.concat " " args ^ ")\n" ^ indent ^ "  " ^ body_str ^ ">"
  | VCont _ -> "<continuation>"
  | VPair (a, b) -> 
      "(" ^ string_of_value ~offset:(offset + 2) a ^ " . " ^ string_of_value ~offset:(offset + 2) b ^ ")"
  | VNil -> "nil"
  | VPrim _ -> "<builtin>"

let print_env env =
  print_endline "Environment:";
  Env.iter (fun k v -> print_endline (k ^ " -> " ^ string_of_value v)) env

(* Helper function to extract number from value *)
let value_to_number = function
  | VNumber n -> n
  | v -> runtime_error (Printf.sprintf "Expected a number, got %s" (string_of_value v))

(* Helper function to extract boolean from value *)
let value_to_bool = function 
  | VBool b -> b 
  | v -> runtime_error (Printf.sprintf "Expected a boolean, got %s" (string_of_value v))

(* Primitive function mapping *)
let primitive_functions =
  let module StringMap = Map.Make (String) in
  (* We have to take List.tl of args because the first argument is the function itself after the closure translation *)
  let aux f = fun args -> f (List.map value_to_number args) in
  StringMap.of_list
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
          | _ -> runtime_error "Expected two arguments for <") );
      ( ">",
        aux (function
          | [ x; y ] -> VBool (x > y)
          | _ -> runtime_error "Expected two arguments for >") );
      ( "<=",
        aux (function
          | [ x; y ] -> VBool (x <= y)
          | _ -> runtime_error "Expected two arguments for <=") );
      ( ">=",
        aux (function
          | [ x; y ] -> VBool (x >= y)
          | _ -> runtime_error "Expected two arguments for >=") );
      ( "cons",
        function
        | [ x; y ] -> VPair (x, y)
        | _ -> runtime_error "Expected two arguments for cons" );
      ( "car",
        function
        | [ VPair (x, _) ] -> x
        | [ v ] -> runtime_error (Printf.sprintf "Expected a pair for car, got %s" (string_of_value v))
        | _ -> runtime_error "Expected one argument for car" );
      ( "cdr",
        function
        | [ VPair (_, y) ] -> y
        | [ v ] -> runtime_error (Printf.sprintf "Expected a pair for cdr, got %s" (string_of_value v))
        | _ -> runtime_error "Expected one argument for cdr" );
      ( "list-ref",
        function
        | [ list; VNumber index ] ->
            let rec aux lst idx =
              match lst with
              | VNil -> runtime_error "list-ref: index out of bounds"
              | VPair (car, cdr) ->
                  if idx = 0 then car else aux cdr (idx - 1)
              | v -> runtime_error (Printf.sprintf "list-ref: expected a list, got %s" (string_of_value v))
            in
            aux list index
        | [ _; v ] -> runtime_error (Printf.sprintf "Expected a number for list-ref index, got %s" (string_of_value v))
        | _ -> runtime_error "Expected a list and a number for list-ref" );
      ("null?", function [ VNil ] -> VBool true | _ -> VBool false);
      ("pair?", function [ VPair _ ] -> VBool true | _ -> VBool false);
      ( "list?",
        let rec aux = function
          | [ VNil ] -> VBool true
          | [ VPair (_, rest) ] -> aux [ rest ]
          | _ -> VBool false
        in
        aux );
      ( "eq?",
        function
        | [ x; y ] -> VBool (x = y)
        | _ -> runtime_error "Expected two arguments for eq?" );
    ]


(* Evalualte a list of expressions sequentially, expressions cannot reference variables in each other similar to regular let*)
let rec eval_exprs :
    env -> Ast.expr list -> value list -> (value list -> value) -> value =
 fun env args acc k ->
  match args with
  | [] -> k acc
  | a :: args -> 
      push_context a "evaluating argument in function call";
      eval_expr env a (fun v -> 
          pop_context ();
          eval_exprs env args (acc @ [ v ]) k)

and extend_env : env -> (string * value) list -> env =
  List.fold_left (fun acc (name, value) -> Env.add name value acc)

(* Main expression evaluation function *)
and eval_expr : env -> Ast.expr -> (value -> value) -> value =
 fun env expr k ->
  push_context expr "evaluating expression";
  let result = 
    match expr.value with
    | Ast.Number n -> k (VNumber n)
    | Ast.Bool b -> k (VBool b)
    | Ast.String s -> k (VString s)
    | Ast.Var sym -> (
        match Env.find_opt sym env with
        | Some value -> k value
        | None -> runtime_error ("Unknown symbol: " ^ sym))
    | Ast.Lambda { ids; body } -> k (VClosure { args = ids; body; env })
    | Ast.App (func_expr :: arg_exprs) ->
        push_context func_expr "evaluating function in application";
        eval_expr env func_expr (fun func_value ->
            pop_context ();
            match func_value with
            | VClosure { args; body; env = captured_env } ->
                eval_exprs env arg_exprs [] (fun arg_values ->
                    if List.length arg_values <> List.length args then
                      runtime_error
                        (Printf.sprintf "Expected %d arguments, got %d"
                           (List.length args) (List.length arg_values))
                    else
                      let extended_env =
                        extend_env captured_env (List.combine args arg_values)
                      in
                      eval_expr extended_env body k)
            | VRecClosure { name; args; body; env = captured_env } ->
                eval_exprs env arg_exprs [] (fun arg_values ->
                    if List.length arg_values <> List.length args then
                      runtime_error
                        (Printf.sprintf "Expected %d arguments, got %d"
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
            | v -> runtime_error (Printf.sprintf "Expected a function, got %s" (string_of_value v)))
    | Ast.App [] -> runtime_error "Empty application"
    | Ast.Callcc { value = Ast.Lambda { ids; body }; _ } -> (
        match ids with
        | [ id ] ->
            let extended_env = Env.add id (VCont k) env in
            eval_expr extended_env body k
        | _ -> runtime_error "Callcc expected a lambda with one argument")
    | Ast.Callcc _ -> runtime_error "Callcc expected a lambda"
    | Ast.Let { defs; body } ->
        eval_exprs env (List.map snd defs) [] (fun values ->
            let extended_env =
              extend_env env (List.combine (List.map fst defs) values)
            in
            eval_expr extended_env body k)
    | Ast.If { cond; y; n } ->
        push_context cond "evaluating condition in if";
        eval_expr env cond (fun condition_value ->
            pop_context ();
            if value_to_bool condition_value then 
              eval_expr env y k
            else 
              eval_expr env n k)
    | Ast.Pair (a, b) ->
        push_context a "evaluating first element of pair";
        eval_expr env a (fun a_value ->
            pop_context ();
            push_context b "evaluating second element of pair";
            eval_expr env b (fun b_value -> 
                pop_context ();
                k (VPair (a_value, b_value))))
    | Ast.Nil -> k VNil
    | Ast.Car e ->
        push_context e "evaluating argument of car";
        eval_expr env e (fun e_value ->
            pop_context ();
            match e_value with
            | VPair (car, _) -> k car
            | v -> runtime_error (Printf.sprintf "car: expected a pair, got %s" (string_of_value v)))
    | Ast.Cdr e ->
        push_context e "evaluating argument of cdr";
        eval_expr env e (fun e_value ->
            pop_context ();
            match e_value with
            | VPair (_, cdr) -> k cdr
            | v -> runtime_error (Printf.sprintf "cdr: expected a pair, got %s" (string_of_value v)))
    | Ast.Cons (e1, e2) ->
        push_context e1 "evaluating first argument of cons";
        eval_expr env e1 (fun e1_value ->
            pop_context ();
            push_context e2 "evaluating second argument of cons";
            eval_expr env e2 (fun e2_value -> 
                pop_context ();
                k (VPair (e1_value, e2_value))))
    | Ast.Prim name -> (
        let module StringMap = Map.Make (String) in
        match StringMap.find_opt name primitive_functions with
        | Some f -> k (VPrim f)
        | None -> runtime_error ("Unknown primitive: " ^ name))
  in
  pop_context ();
  result

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
      (fun (acc, env) (expr : Ast.top_expr) ->
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

(* Evaluate a list of top-level expressions and return both results and environment *)
let eval_file_with_env : Ast.top_expr list -> env -> value list * env =
 fun exprs env ->
  let results, final_env =
    List.fold_left
      (fun (acc, env) (expr : Ast.top_expr) ->
        match expr.value with
        | Ast.Define { name; expr } ->
            let updated_env = process_definition name expr env in
            (acc, updated_env)
        | Ast.Expr expr ->
            let v = eval_expr env expr (fun v -> v) in
            (v :: acc, env))
      ([], env) exprs
  in
  (List.rev results, final_env)

(* Create initial environment with builtin definitions *)
let create_initial_env () =
  let module StringMap = Map.Make (String) in
  let builtin_env = Env.empty in
  let builtin_defs = Builtins.builtin_definitions in
  let _, final_env = eval_file_with_env builtin_defs builtin_env in
  final_env

(* Main interpreter entry point *)
let interpret files =
  let filename = List.hd files in
  let channel = open_in filename in
  let lexbuf = Lexing.from_channel channel in

  try
    let parse_tree = Parser.parse Lexer.lex lexbuf in
    let results = eval_file parse_tree (create_initial_env ()) in
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

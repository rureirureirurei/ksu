(* Interpreter module for KSU language *)
open Compiler_lib

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
      "<function (" ^ String.concat " " args ^ ")\n" ^ indent ^ "  " ^ body_str
      ^ ">"
  | VRecClosure { name; args; body; _ } ->
      let indent = String.make offset ' ' in
      let body_str = Ast.string_of_expr body in
      "<rec-function " ^ name ^ " (" ^ String.concat " " args ^ ")\n" ^ indent
      ^ "  " ^ body_str ^ ">"
  | VCont _ -> "<continuation>"
  | VPair (a, b) ->
      "("
      ^ string_of_value ~offset:(offset + 2) a
      ^ " . "
      ^ string_of_value ~offset:(offset + 2) b
      ^ ")"
  | VNil -> "nil"
  | VPrim _ -> "<builtin>"

let print_env env =
  print_endline "Environment:";
  Env.iter (fun k v -> print_endline (k ^ " -> " ^ string_of_value v)) env

(* Helper function to extract number from value *)
let value_to_number = function
  | VNumber n -> n
  | v ->
      failwith (Printf.sprintf "Expected a number, got %s" (string_of_value v))

(* Helper function to extract boolean from value *)
let value_to_bool = function
  | VBool b -> b
  | v ->
      failwith (Printf.sprintf "Expected a boolean, got %s" (string_of_value v))

(* Primitive function mapping *)
let builtin_functions =
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
        function
        | [ x; y ] -> VPair (x, y)
        | _ -> failwith "Expected two arguments for cons" );
      ( "car",
        function
        | [ VPair (x, _) ] -> x
        | [ v ] ->
            failwith
              (Printf.sprintf "Expected a pair for car, got %s"
                 (string_of_value v))
        | _ -> failwith "Expected one argument for car" );
      ( "cdr",
        function
        | [ VPair (_, y) ] -> y
        | [ v ] ->
            failwith
              (Printf.sprintf "Expected a pair for cdr, got %s"
                 (string_of_value v))
        | _ -> failwith "Expected one argument for cdr" );
      ( "list-ref",
        function
        | [ list; VNumber index ] ->
            let rec aux lst idx =
              match lst with
              | VNil -> failwith "list-ref: index out of bounds"
              | VPair (car, cdr) -> if idx = 0 then car else aux cdr (idx - 1)
              | v ->
                  failwith
                    (Printf.sprintf "list-ref: expected a list, got %s"
                       (string_of_value v))
            in
            aux list index
        | [ _; v ] ->
            failwith
              (Printf.sprintf "Expected a number for list-ref index, got %s"
                 (string_of_value v))
        | _ -> failwith "Expected a list and a number for list-ref" );
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
        | _ -> failwith "Expected two arguments for eq?" );
    ]

(* Evalualte a list of expressions separately, expressions cannot reference variables in each other similar to regular let*)
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
 fun env expr k ->
  let result =
    match expr.value with
    | Ast.Number n -> k (VNumber n)
    | Ast.Bool b -> k (VBool b)
    | Ast.String s -> k (VString s)
    | Ast.Var sym -> (
        match Env.find_opt sym env with
        | Some value -> k value
        | None -> failwith ("Unknown symbol: " ^ sym))
    | Ast.Lambda { ids; body } -> k (VClosure { args = ids; body; env })
    | Ast.App { func; args = arg_exprs } ->
        eval_expr env func (fun func_value ->
            match func_value with
            | VClosure { args; body; env = captured_env } ->
                eval_exprs env arg_exprs [] (fun arg_values ->
                    if List.length arg_values <> List.length args then
                      failwith
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
                      failwith
                        (Printf.sprintf "Expected %d arguments, got %d"
                           (List.length args) (List.length arg_values))
                    else
                      let extended_env =
                        Env.add name func_value
                          (extend_env captured_env
                             (List.combine args arg_values))
                      in
                      eval_expr extended_env body k)
            | VCont f ->
                eval_exprs env arg_exprs [] (fun arg_values ->
                    f (List.hd arg_values))
            | VPrim f ->
                eval_exprs env arg_exprs [] (fun arg_values -> k (f arg_values))
            | v ->
                failwith
                  (Printf.sprintf "Expected a function, got %s"
                     (string_of_value v)))
    | Ast.Callcc { value = Ast.Lambda { ids; body }; _ } -> (
        match ids with
        | [ id ] ->
            let extended_env = Env.add id (VCont k) env in
            eval_expr extended_env body k
        | _ -> failwith "Callcc expected a lambda with one argument")
    | Ast.Callcc _ -> failwith "Callcc expected a lambda"
    | Ast.Let { defs; body } ->
        let rec aux = (fun env (args: (Ast.var * Ast.expr) list) acc k ->
          match args with
          | [] -> k acc
          | (id, def) :: args -> 
            eval_expr env def (fun v -> aux (Env.add id v env) args (acc @ [ v ]) k))
        in 
        aux env defs [] (fun values ->
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
    | Ast.Car e ->
        eval_expr env e (fun e_value ->
            match e_value with
            | VPair (car, _) -> k car
            | v ->
                failwith
                  (Printf.sprintf "car: expected a pair, got %s"
                     (string_of_value v)))
    | Ast.Cdr e ->
        eval_expr env e (fun e_value ->
            match e_value with
            | VPair (_, cdr) -> k cdr
            | v ->
                failwith
                  (Printf.sprintf "cdr: expected a pair, got %s"
                     (string_of_value v)))
    | Ast.Prim name -> (
        let module StringMap = Map.Make (String) in
        match StringMap.find_opt name builtin_functions with
        | Some f -> k (VPrim f)
        | None -> failwith ("Unknown primitive: " ^ name))
  in
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
let eval : Ast.top_expr list -> env -> value list * env =
 fun exprs env ->
  List.fold_left
    (fun (acc, env) (expr : Ast.top_expr) ->
      match expr.value with
      | Ast.Define { name; expr } ->
          let updated_env = process_definition name expr env in
          (acc, updated_env)
      | Ast.Expr expr ->
          let v = eval_expr env expr (fun v -> v) in
          (acc @ [ v ], env))
    ([], env) exprs

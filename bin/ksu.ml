(* Main entry point for KSU language *)
open Compiler_lib
open Compiler

(* Command line argument parsing *)
let usage_msg = "ksu [--repl] <files>..."
let repl_mode = ref false
let input_files = ref []
let anon_fun filename = input_files := filename :: !input_files

let speclist =
  [ ("--repl", Arg.Set repl_mode, "Start interactive REPL mode") ]

let () =
  Arg.parse speclist anon_fun usage_msg;

  (* Initialize environment with builtin definitions *)
  let builtin_ast = Builtins.builtin_definitions in
  let builtin_ast_converted = Closures.t_file builtin_ast in
  let _, env = Interpreter.eval builtin_ast_converted Interpreter.Env.empty in

  (* Parse provided files *)
  let files = List.rev !input_files in
  let files_asts = List.map (fun file -> Parser.parse Lexer.lex (Lexing.from_channel (open_in file))) files in
  let files_converted_asts = List.concat @@ List.map Closures.t_file files_asts in
  let results, _ = Interpreter.eval files_converted_asts env in
  
  (* Start REPL if --repl flag is provided *)
  if !repl_mode then
    failwith "REPL mode not implemented"
  else
    List.iter (fun result -> print_endline (Interpreter.string_of_value result)) results


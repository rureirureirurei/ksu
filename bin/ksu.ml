(* Main entry point for KSU language *)
open Compiler_lib
open Compiler

(* Command line argument parsing *)
let usage_msg = "ksu [--repl] [--debug] <files>..."
let debug_mode = ref false
let repl_mode = ref false
let input_files = ref []
let anon_fun filename = input_files := filename :: !input_files
let speclist = [ ("--repl", Arg.Set repl_mode, "Start interactive REPL mode"); ("--debug", Arg.Set debug_mode, "Print debug information") ]

let () =
  Arg.parse speclist anon_fun usage_msg;

  (* Initialize environment with builtin definitions *)
  let builtin_ast = Builtins.builtin_definitions in

  (* Parse provided files *)
  let files = List.rev !input_files in
  let parse_file =
   fun file acc ->
    acc @ Parser.parse Lexer.lex (Lexing.from_channel (open_in file))
  in
  let files_asts = List.fold_right parse_file files [] in
  let files_converted_asts =
    Closures.t_file builtin_ast @ Closures.t_file files_asts
  in

  let files_flattened_asts = List.map Flattening.disambiguate_top_expr files_converted_asts in
  let files_flattened_asts = List.map Flattening.flatten_top_expr files_flattened_asts in
  
  if (!debug_mode) then (
    List.iter (fun ast -> print_endline @@ "\n" ^ (Ast.string_of_top_expr ast)) files_flattened_asts
  ) else ();

  let results, _ =
    Interpreter.eval files_flattened_asts Interpreter.Env.empty
  in

  (* Start REPL if --repl flag is provided *)
  if !repl_mode then failwith "REPL mode not implemented"
  else
    List.iter
      (fun result -> print_endline (Interpreter.string_of_value result))
      results

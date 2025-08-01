(* Main entry point for KSU language *)
open Compiler_lib
open Compiler

(* REPL implementation *)
let start_repl () =
  Printf.printf "KSU REPL - Interactive Mode\n";
  Printf.printf "Type expressions to evaluate (Ctrl+C to exit)\n";
  Printf.printf "Examples:\n";
  Printf.printf "  (+ 1 2)\n";
  Printf.printf "  (define x 5)\n";
  Printf.printf "  (if (> x 3) \"yes\" \"no\")\n\n";

  (* Initialize environment *)
  let rec loop env =
    try
      Printf.printf "ksu> ";
      flush stdout;

      let input = read_line () in

      (* Handle empty input *)
      if String.trim input = "" then loop env
      else
        try
          (* Parse the input *)
          let lexbuf = Lexing.from_string input in
          let parse_tree = Parser.parse Lexer.lex lexbuf in

          (* Evaluate each expression in the input *)
          let results, new_env =
            List.fold_left
              (fun (acc, current_env) (expr: Ast.top_expr) ->
                match expr.value with
                | Ast.Define { name; expr } ->
                    let updated_env =
                      Interpreter.process_definition name expr current_env
                    in
                    (acc, updated_env)
                | Ast.Expr expr ->
                    let v =
                      Interpreter.eval_expr current_env expr (fun v -> v)
                    in
                    (v :: acc, current_env))
              ([], env) parse_tree
          in

          (* Print results *)
          List.iter
            (fun result ->
              Printf.printf "%s\n" (Interpreter.string_of_value result))
            (List.rev results);

          (* Continue with updated environment *)
          loop new_env
        with
        | Parser.Error ->
            Printf.eprintf "Parser error: Invalid syntax\n";
            loop env
        | Failure msg ->
            Printf.eprintf "Runtime error: %s\n" msg;
            loop env
        | Sys.Break ->
            Printf.printf "\n";
            loop env
        | End_of_file ->
            Printf.printf "\nGoodbye!\n";
            exit 0
    with
    | Sys.Break ->
        Printf.printf "\n";
        loop env
    | End_of_file ->
        Printf.printf "\nGoodbye!\n";
        exit 0
  in

  loop Interpreter.init_env

let repl () = start_repl ()

(* Closure conversion mode *)
let closure_conversion_mode () =
  Printf.printf "KSU Closure Conversion Mode\n";
  Printf.printf "Type expressions to parse and convert (Ctrl+C to exit)\n";
  Printf.printf "Examples:\n";
  Printf.printf "  (lambda (x) (+ x 1))\n";
  Printf.printf "  (define f (lambda (x y) (+ x y)))\n\n";

  let rec loop () =
    try
      Printf.printf "ksu-closure> ";
      flush stdout;

      let input = read_line () in

      (* Handle empty input *)
      if String.trim input = "" then loop ()
      else
        try
          (* Parse the input *)
          let lexbuf = Lexing.from_string input in
          let parse_tree = Parser.parse Lexer.lex lexbuf in

          (* Apply closure conversion *)
          let converted_tree = Closures.t_file parse_tree in

          (* Print the converted AST *)
          List.iter
            (fun expr ->
              Printf.printf "%s\n" (Ast.string_of_top_expr expr))
            converted_tree;

          (* Continue *)
          loop ()
        with
        | Parser.Error ->
            Printf.eprintf "Parser error: Invalid syntax\n";
            loop ()
        | Failure msg ->
            Printf.eprintf "Closure conversion error: %s\n" msg;
            loop ()
        | Sys.Break ->
            Printf.printf "\n";
            loop ()
        | End_of_file ->
            Printf.printf "\nGoodbye!\n";
            exit 0
    with
    | Sys.Break ->
        Printf.printf "\n";
        loop ()
    | End_of_file ->
        Printf.printf "\nGoodbye!\n";
        exit 0
  in

  loop ()

let compile ~output files =
  Printf.printf "Compiling to %s with files: %s\n" output
    (String.concat ", " files);
  failwith "Compiler not implemented"

(* Command line argument parsing *)
let usage_msg = "ksu [--compile] [--output <file>] [--closure] <files>..."
let compile_mode = ref false
let closure_mode = ref false
let output_file = ref ""
let input_files = ref []
let anon_fun filename = input_files := filename :: !input_files

let speclist =
  [
    ("--compile", Arg.Set compile_mode, "Compile to native executable");
    ("--output", Arg.Set_string output_file, "Output file for compilation");
    ("--closure", Arg.Set closure_mode, "Run in closure conversion mode");
  ]

let () =
  Arg.parse speclist anon_fun usage_msg;

  let files = List.rev !input_files in

  match (!compile_mode, !closure_mode, files) with
  | true, _, [] ->
      Printf.eprintf "Error: No input files specified for compilation\n";
      Printf.eprintf "%s\n" usage_msg;
      exit 1
  | true, _, _ ->
      if !output_file = "" then (
        Printf.eprintf "Error: --output flag required for compilation\n";
        Printf.eprintf "%s\n" usage_msg;
        exit 1);
      compile ~output:!output_file files
  | _, true, [] -> closure_conversion_mode ()
  | _, true, _ ->
      (* Process files in closure conversion mode *)
      List.iter
        (fun filename ->
          try
            let channel = open_in filename in
            let lexbuf = Lexing.from_channel channel in
            let parse_tree = Parser.parse Lexer.lex lexbuf in
            let converted_tree = Closures.t_file parse_tree in
            
            Printf.printf "=== Closure conversion for %s ===\n" filename;
            List.iter
              (fun expr ->
                Printf.printf "%s\n" (Ast.string_of_top_expr expr))
              converted_tree;
            Printf.printf "\n";
            
            close_in channel
          with
          | Parser.Error ->
              Printf.eprintf "Parse error in %s\n" filename;
              exit 1
          | Sys_error msg ->
              Printf.eprintf "Error opening file %s: %s\n" filename msg;
              exit 1
          | e ->
              Printf.eprintf "Unexpected error in %s: %s\n" filename (Printexc.to_string e);
              exit 1)
        files
  | false, false, [] -> repl ()
  | false, false, _ -> Interpreter.interpret files

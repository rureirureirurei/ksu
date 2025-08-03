(* Main entry point for KSU language *)
open Compiler_lib
open Compiler
open Builtins
(* REPL implementation *)
let start_repl_with_env env =
  Printf.printf "KSU REPL - Interactive Mode\n";
  Printf.printf "Type expressions to evaluate (Ctrl+C to exit)\n";

  (* Initialize environment *)
  let rec loop env =
    try
      Printf.printf "ksu repl> ";
      flush stdout;

      let input = read_line () in

      (* Handle empty input *)
      if String.trim input = "" then loop env
      else
        try
          (* Parse the input *)
          let lexbuf = Lexing.from_string input in
          let parse_tree = Parser.parse Lexer.lex lexbuf in
          print_endline "=== Parsed tree ===";
          List.iter
            (fun expr -> Printf.printf "%s\n\n" (Ast.string_of_top_expr expr))
            parse_tree;
          let converted_tree = Closures.t_file parse_tree in

          print_endline "=== Converted tree ===";
          List.iter
            (fun expr -> Printf.printf "%s\n\n" (Ast.string_of_top_expr expr))
            converted_tree;
          print_endline "=== === ===";

          (* Evaluate each expression in the input *)
          let results, new_env =
            List.fold_left
              (fun (acc, current_env) (expr : Ast.top_expr) ->
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
              ([], env) converted_tree
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
            flush stderr;
            loop env
        | Failure msg ->
            Printf.eprintf "Runtime error: %s\n" msg;
            flush stderr;
            loop env
        | Sys.Break ->
            Printf.printf "Sys break \n";
            loop env
        | End_of_file -> exit 0
    with
    | Sys.Break ->
        Printf.printf "Sys break \n";
        loop env
    | End_of_file -> exit 0
  in
  loop env

  (* Command line argument parsing *)
let usage_msg = "ksu [--closure] [--repl] <files>..."
let closure_mode = ref false
let repl_mode = ref false
let input_files = ref []
let anon_fun filename = input_files := filename :: !input_files

let speclist =
  [
    ( "--closure",
      Arg.Set closure_mode,
      "Runs interpreter on the closure converted code" );
    ("--repl", Arg.Set repl_mode, "Start interactive REPL mode");
  ]

let () =
  Arg.parse speclist anon_fun usage_msg;

  let files = List.rev !input_files in

  match (!closure_mode, !repl_mode, files) with
  | true, false, _ ->
      (* Process files in closure conversion mode *)
      List.iter
        (fun filename ->
          try
            let channel = open_in filename in
            let lexbuf = Lexing.from_channel channel in
            let parse_tree = Parser.parse Lexer.lex lexbuf in
            Printf.printf "=== Parsed %s ===\n" filename;
            List.iter
              (fun expr -> Printf.printf "%s\n" (Ast.string_of_top_expr expr))
              parse_tree;
            let converted_tree = Closures.t_file (builtin_definitions @ parse_tree) in

            Printf.printf "\n=== Closure conversion  %s ===\n" filename;
            List.iter
              (fun expr -> Printf.printf "%s\n\n" (Ast.string_of_top_expr expr))
              converted_tree;
            Printf.printf "\n";

            Printf.printf "\n=== Results %s ===\n" filename;

            let results =
              Interpreter.eval_file converted_tree Interpreter.Env.empty
            in
            List.iter
              (fun result -> print_endline (Interpreter.string_of_value result))
              results
          with
          | Parser.Error ->
              Printf.eprintf "Parse error in %s\n" filename;
              exit 1
          | Sys_error msg ->
              Printf.eprintf "Error opening file %s: %s\n" filename msg;
              exit 1
          | e ->
              Printf.eprintf "Unexpected error in %s: %s\n" filename
                (Printexc.to_string e);
              exit 1)
        files
  | true, true, _ -> 
    (* Start REPL with initial files *)
    let final_env = 
      List.fold_left
        (fun env filename ->
          try
            let channel = open_in filename in
            let lexbuf = Lexing.from_channel channel in
            let parse_tree = Parser.parse Lexer.lex lexbuf in
            Printf.printf "=== Loading %s ===\n" filename;
            let converted_tree = Closures.t_file (builtin_definitions @ parse_tree) in
            Printf.printf "=== Closure conversion %s ===\n" filename;
            List.iter
              (fun expr -> Printf.printf "%s\n\n" (Ast.string_of_top_expr expr))
              converted_tree;
            Printf.printf "\n";
            let results, new_env =
              Interpreter.eval_file_with_env converted_tree env
            in
            List.iter
              (fun result -> print_endline (Interpreter.string_of_value result))
              results;
            Printf.printf "=== File %s loaded successfully ===\n\n" filename;
            new_env
          with
          | Parser.Error ->
              Printf.eprintf "Parse error in %s\n" filename;
              exit 1
          | Sys_error msg ->
              Printf.eprintf "Error opening file %s: %s\n" filename msg;
              exit 1
          | e ->
              Printf.eprintf "Unexpected error in %s: %s\n" filename
                (Printexc.to_string e);
              exit 1)
        (let _, initial_env = 
           Interpreter.eval_file_with_env (Closures.t_file builtin_definitions) Interpreter.Env.empty
         in
          Interpreter.print_env initial_env;
          initial_env)
        files
    in
    start_repl_with_env final_env
  | false, _, _ -> failwith "We don't support non-closure mode"

(* Stub functions for different modes *)
let repl () = 
  Printf.printf "KSU REPL - Not implemented yet\n";
  Printf.printf "Type expressions to evaluate (Ctrl+C to exit)\n";
  failwith "REPL not implemented"

let compile ~output files =
  Printf.printf "Compiling to %s with files: %s\n" 
    output (String.concat ", " files);
  failwith "Compiler not implemented"

let interpret files =
  Printf.printf "Interpreting files: %s\n" 
    (String.concat ", " files);
  failwith "Interpreter not implemented"

(* Command line argument parsing *)
let usage_msg = "ksu [--compile] [--output <file>] <files>..."

let compile_mode = ref false
let output_file = ref ""
let input_files = ref []

let anon_fun filename = 
  input_files := filename :: !input_files

let speclist = [
  ("--compile", Arg.Set compile_mode, "Compile to native executable");
  ("--output", Arg.Set_string output_file, "Output file for compilation");
]

let () =
  Arg.parse speclist anon_fun usage_msg;
  
  let files = List.rev !input_files in
  
  match (!compile_mode, files) with
  | (true, []) ->
      Printf.eprintf "Error: No input files specified for compilation\n";
      Printf.eprintf "%s\n" usage_msg;
      exit 1
  | (true, _) ->
      if !output_file = "" then begin
        Printf.eprintf "Error: --output flag required for compilation\n";
        Printf.eprintf "%s\n" usage_msg;
        exit 1
      end;
      compile ~output:!output_file files
  | (false, []) ->
      repl ()
  | (false, _) ->
      interpret files
  


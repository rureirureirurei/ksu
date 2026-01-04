(* Main entry point for KSU language *)
open Ksu_parser
open Compiler

(* Command line argument parsing *)
let usage_msg = "ksu <file>"
let input_file = ref None

let anon_fun filename =
  match !input_file with
  | None -> input_file := Some filename
  | Some _ -> failwith "Error: Only one file can be specified"

let () =
  Arg.parse [] anon_fun usage_msg;

  (* Check that exactly one file was provided *)
  match !input_file with
  | None ->
      print_endline "Error: No file specified";
      print_endline usage_msg;
      exit 1
  | Some file ->
      (* Parse the file *)
      let lexbuf = Lexing.from_channel (open_in file) in
      let ast =
        try Builtins.builtin_definitions @ Parser.parse Lexer.lex lexbuf
        with Parser.Error ->
          let pos = lexbuf.lex_curr_p in
          Printf.eprintf ("Parsing error at line %d, column %d\n, file %s") pos.pos_lnum
            (pos.pos_cnum - pos.pos_bol) file;
          []
      in

      (* Sanitize variable names for C code generation *)
      let sanitized_ast = Name_sanitizer.sanitize_top_exprs ast in

      (* Do closure conversion *)
      let converted_ast = Closures.convert sanitized_ast in

      (* Generate C code *)
      let c_text = Ksu2c.ksu2c converted_ast in

      (* Print the result *)
      print_endline c_text

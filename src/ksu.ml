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
        try Lang.Ast.builtin_definitions @ Parser.parse Lexer.lex lexbuf
        with Parser.Error ->
          let pos = lexbuf.lex_curr_p in
          Printf.eprintf ("Parsing error at line %d, column %d, file %s\n") pos.pos_lnum
            (pos.pos_cnum - pos.pos_bol) file;
          exit 1
      in

      (* Sanitize variable names for C code generation *)
      let sanitized_ast = Name_sanitizer.sanitize_top_exprs ast in

      (* Write AST to debug file *)
      let ast_file = "/tmp/" ^ Filename.basename file ^ ".ast" in
      let oc = open_out ast_file in
      List.iter (fun e -> output_string oc (Lang.Ast.string_of_top_expr e ^ "\n")) sanitized_ast;
      close_out oc;

      (* Do CPS conversion *)
      let cps_ast = List.map Cps.t_top sanitized_ast in

      (* Back-translate from CPS to AST *)
      let from_cps_ast = List.map Cps.from_cps_top cps_ast in

      (* Write CPS-converted AST to debug file *)
      let cps_ast_file = "/tmp/" ^ Filename.basename file ^ ".cps.ast" in
      let oc = open_out cps_ast_file in
      List.iter (fun e -> output_string oc (Lang.Ast.string_of_top_expr e ^ "\n")) from_cps_ast;
      close_out oc;

      (* Do closure conversion *)
      let converted_ast = Closures.convert from_cps_ast in

      (* Write closure-converted AST to debug file *)
      let cc_ast_file = "/tmp/" ^ Filename.basename file ^ ".cc.ast" in
      let oc = open_out cc_ast_file in
      List.iter (fun e -> output_string oc (Closures.string_of_cc_top_expr e ^ "\n")) converted_ast;
      close_out oc;

      (* Generate C code *)
      let c_text = Ksu2c.ksu2c converted_ast in

      (* Print the result *)
      print_endline c_text

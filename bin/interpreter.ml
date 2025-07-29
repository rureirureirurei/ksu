(* Interpreter module for KSU language *)
open Compiler_lib

type token = Parser.token

let string_of_token: token -> string =
  function
  | BOOL b -> "BOOL(" ^ string_of_bool b ^ ")"
  | CALLCC -> "CALLCC"
  | DEFINE -> "DEFINE"
  | EOF -> "EOF"
  | IF -> "IF"
  | LAMBDA -> "LAMBDA"
  | LBRACKET -> "LBRACKET"
  | LET -> "LET"
  | LPAREN -> "LPAREN"
  | NUMBER n -> "NUMBER(" ^ string_of_int n ^ ")"
  | RBRACKET -> "RBRACKET"
  | RPAREN -> "RPAREN"
  | STRING s -> "STRING(\"" ^ s ^ "\")"
  | SYMBOL s -> "SYMBOL(" ^ s ^ ")"

let print_all_tokens lexbuf =  
  let rec aux () = 
    match Lexer.lex lexbuf with
    | EOF -> ()
    | _ as token -> (
      print_endline (string_of_token token);
      aux ()
    )
  in aux ()

let interpret files =
  Printf.printf "Interpreting files: %s\n" 
    (String.concat ", " files);
  let channel = open_in (List.hd files) in
  let lexbuf = Lexing.from_channel channel in 
  let parse_tree = Parser.parse Lexer.lex lexbuf in 
    print_endline (Ast.string_of_expr parse_tree)
    
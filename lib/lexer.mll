{ 
  open Parser
  type token = Parser.token

  let string_of_token = function
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
}

let digit = ['0' - '9']
let letter = ['a' - 'z' 'A' - 'Z']
let symbol_char = letter | ['+' '-' '*' '/' '<' '>' '=' '!' '?']
let whitespace = [' ' '\t' '\n' '\r']

rule lex = parse
  | digit+ as n { NUMBER (int_of_string n) }
  | '"' ([^ '"']* as s) '"' { STRING s }
  | "#t" { BOOL true }
  | "#f" { BOOL false }
  | '(' { LPAREN }
  | ')' { RPAREN }
  | '[' { LBRACKET }
  | ']' { RBRACKET }
  | "define" { DEFINE }
  | "lambda" { LAMBDA }
  | "call/cc" { CALLCC }
  | ';' [^ '\n']* { lex lexbuf }
  | symbol_char (symbol_char | digit)* as s { SYMBOL s }
  | whitespace+ { lex lexbuf }
  | eof { EOF }

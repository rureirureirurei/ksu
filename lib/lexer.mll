{ 
  open Parser
  type token = Parser.token
}

let digit = ['0' - '9']
let letter = ['a' - 'z' 'A' - 'Z']
let symbol_char = letter | ['+' '-' '*' '/' '<' '>' '=' '!' '?']
let whitespace = [' ' '\t' '\n' '\r']

rule lex = parse
  | digit+ as n { NUMBER (int_of_string n) }
  | '"' ([^ '"']* as s) '"' { STRING s }
  | symbol_char (symbol_char | digit)* as s { SYMBOL s }
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
  | whitespace+ { lex lexbuf }
  | eof { EOF }

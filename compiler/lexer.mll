{ 
  open Parser
  type token = Parser.token
}

let digit = ['0' - '9']
let letter = ['a' - 'z' 'A' - 'Z']
let symbol_char = letter | ['+' '-' '*' '/' '<' '>' '=' '!' '?']
let whitespace = [' ' '\t']
let newline = ['\n' '\r']

rule lex = parse
  | digit+ as n { NUMBER (int_of_string n) }
  | '"' ([^ '"']* as s) '"' { STRING s }
  | "#t" { BOOL true }
  | "#f" { BOOL false }
  | '(' { LPAREN }
  | ')' { RPAREN }
  | '[' { LBRACKET }
  | ']' { RBRACKET }
  | "let" { LET }
  | "if" { IF }
  | "define" { DEFINE }
  | "lambda" { LAMBDA }
  | "call/cc" { CALLCC }
  | "'" { QUOTE }
  | ';' [^ '\n' '\r']* { lex lexbuf }
  | newline { 
      Lexing.new_line lexbuf; 
      lex lexbuf 
    }
  | symbol_char (symbol_char | digit)* as s { SYMBOL s }
  | whitespace+ { lex lexbuf }
  | eof { EOF }

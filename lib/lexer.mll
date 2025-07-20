{ (* Header is just some arbitrary OCaml code *)
     open Parser
     type token = Parser.token
}

(* Then, here we might define some aux regular expressions.

let somerexpr1 = ...
let somerexpr2 = ...

examples may be 

let lol = "true" | "false"
let kek = ['0'-'9']+
...
*)

let lowercase_letter = ['a' - 'z']
let digit = ['0' - '9']
let ident = lowercase_letter (digit | lowercase_letter)* 
let BOOL_LITERALeral = digit+

rule token = parse
  (* Arithmetic operators *)
  | '+' -> { PLUS }
  | '-' -> { MINUS }
  | '*' -> { TIMES }
  | '/' -> { DIV }
  | '%' -> { MOD }

  (* Comparison operators *)
  | "==" -> { EQUAL }
  | "!=" -> { NOT_EQUAL }
  | '<' -> { LESS }
  | '>' -> { GREATER }
  | "<=" -> { LESS_OR_EQUAL }
  | ">=" -> { GREATER_OR_EQUAL }

  (* Logical operators *)
  | "or" -> { OR }
  | "and" -> { AND }
  | "not" -> { NOT }

  (* Keywords *)
  | "let" -> { LET }
  | "in" -> { IN }
  | "if" -> { IF }
  | "then" -> { THEN }
  | "else" -> { ELSE }
  | "fun" -> { FUN }

  (* Punctuation *)
  | '(' -> { LEFT_PAREN }
  | ')' -> { RIGHT_PAREN }
  | ':' -> { SEMI }
  | "->" -> { ARR }

  (* Types *)
  | "int" -> { INT_T }
  | "bool" -> { BOOL_T }

  (* Literals *)
  | "true" -> { BOOL_LITERAL true }
  | "false" -> { BOOL_LITERAL false }
  | BOOL_LITERALeral as i -> { BOOL_LITERAL (int_of_string i) }
  | ident as id -> { ID id }

  (* Whitespace and newlines *)
  | ['\n'] { new_line lexbuf; token lexbuf }
  | [' ' '\t' '\r'] { token lexbuf }

  (* End of file *)
  | eof -> { EOF }

{ (* Footer is just some arbitrary OCaml code *)
}

%token <bool> BOOL
%token <int> NUMBER
%token <string> SYMBOL
%token <string> STRING
%token LPAREN RPAREN
%token LBRACKET RBRACKET
%token DEFINE
%token IF LAMBDA CALLCC
%token LET
%token BEGIN
%token EOF

%start <Lang.Ast.top_expr list> parse

%{
  open Lang
  open Ast
%}

%%
parse: top_exprs EOF { $1 }

top_exprs:
  | { [] }
  | expr top_exprs { E_Expr $1 :: $2 }
  | LPAREN define_expr RPAREN top_exprs { $2 :: $4 }

expr:
  | atom { $1 }
  | LPAREN compound RPAREN { $2 }

atom:
  | BOOL { E_Bool $1 }
  | NUMBER { E_Number $1 }
  | STRING { E_String $1 }
  | SYMBOL { E_Var $1 }

compound:
  | lambda_expr { $1}
  | if_expr { $1 }
  | let_expr { $1 }
  | begin_expr { $1 }
  | callcc_expr { $1 }
  | exprs {
      match $1 with
      | f :: args -> E_App (f, args)
      | [] -> failwith "Empty application"
    }
  
begin_expr:
  | BEGIN exprs { E_Begin $2 }

lambda_args:
  | { [] }
  | SYMBOL lambda_args { $1 :: $2 }

lambda_expr:
  | LAMBDA LPAREN lambda_args RPAREN expr { E_Lambda ($3, $5) }

if_expr:
  | IF expr expr expr { E_If ($2, $3, $4) }

exprs:
  | expr { [$1] }
  | expr exprs { $1 :: $2 }

callcc_expr:
  | CALLCC SYMBOL expr { E_Callcc ($2, $3) }

let_args: 
  | { [] }
  | LBRACKET SYMBOL expr RBRACKET let_args { ($2, $3) :: $5 }

let_expr:
  | LET LPAREN let_args RPAREN expr { E_Let ($3, $5) }

define_expr:
  | DEFINE SYMBOL expr { E_Define ($2, $3) }
  | DEFINE LPAREN SYMBOL lambda_args RPAREN expr { E_Define ($3, E_Lambda ($4, $6)) }

%token <bool> BOOL
%token <int> NUMBER
%token <string> SYMBOL
%token <string> STRING
%token LPAREN RPAREN
%token LBRACKET RBRACKET
%token DEFINE
%token IF LAMBDA CALLCC
%token LET
%token QUOTE
%token EOF

%start <Compiler_lib.Ast.expr list> parse

%%
parse: exprs EOF { $1 }

exprs:
  | { [] }
  | expr exprs { $1 :: $2 }

expr:
  | atom { $1 }
  | LPAREN compound RPAREN { $2 }

atom:
  | BOOL { Compiler_lib.Ast.Bool $1 }
  | NUMBER { Compiler_lib.Ast.Number $1 }
  | STRING { Compiler_lib.Ast.String $1 }
  | SYMBOL { Compiler_lib.Ast.Symbol $1 }
  | list_expr { $1 }

compound:
  | app_expr { Compiler_lib.Ast.App $1 }
  | lambda_expr { $1}
  | if_expr { $1 }
  | let_expr { $1 }
  | callcc_expr { $1 }
  | define_expr { $1 }

list_expr:
  | QUOTE LPAREN list_elements RPAREN { $3 }

list_elements:
  | { Compiler_lib.Ast.Nil }
  | expr list_elements { Compiler_lib.Ast.Pair ($1, $2) }

lambda_args:
  | SYMBOL { [$1] }
  | SYMBOL lambda_args { $1 :: $2 }

lambda_expr:
  | LAMBDA LPAREN lambda_args RPAREN expr { Compiler_lib.Ast.Lambda { ids = $3; body = $5 } }

if_expr:
  | IF expr expr expr { Compiler_lib.Ast.If { cond = $2; y = $3; n = $4 } }

app_expr:
  | expr { [$1] }
  | expr app_expr { $1 :: $2 }

callcc_expr:
  | CALLCC expr { Compiler_lib.Ast.Callcc $2 }

let_args: 
  | LBRACKET SYMBOL expr RBRACKET { [($2, $3)] }
  | LBRACKET SYMBOL expr RBRACKET let_args { ($2, $3) :: $5 }

let_expr:
  | LET LPAREN let_args RPAREN expr { Compiler_lib.Ast.Let { defs = $3; body = $5 } }

define_expr:
  | DEFINE SYMBOL expr { Compiler_lib.Ast.Define { name = $2; expr = $3 } }
  | DEFINE LPAREN SYMBOL lambda_args RPAREN expr { Compiler_lib.Ast.Define { name = $3; expr = Compiler_lib.Ast.Lambda { ids = $4; body = $6 }  } }

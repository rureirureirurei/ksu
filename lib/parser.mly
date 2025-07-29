%token <bool> BOOL
%token <int> NUMBER
%token <string> SYMBOL
%token <string> STRING
%token LPAREN RPAREN
%token LBRACKET RBRACKET
%token DEFINE
%token IF LAMBDA CALLCC
%token LET
%token EOF

%start <Ast.expr> parse

%%
parse: exprs EOF { Ast.List $1 }

exprs:
  | { [] }
  | expr exprs { $1 :: $2 }

expr:
  | atom { $1 }
  | LPAREN compound RPAREN { $2 }

atom:
  | BOOL { Ast.Bool $1 }
  | NUMBER { Ast.Number $1 }
  | STRING { Ast.String $1 }
  | SYMBOL { Ast.Symbol $1 }

compound:
  | list_expr { Ast.List $1 }
  | lambda_expr { $1}
  | if_expr { $1 }
  | let_expr { $1 }
  | callcc_expr { $1 }
  | define_expr { $1 }
lambda_args:
  | SYMBOL { [$1] }
  | SYMBOL lambda_args { $1 :: $2 }

lambda_expr:
  | LAMBDA LPAREN lambda_args RPAREN expr { Ast.Lambda { ids = $3; b = $5 } }

if_expr:
  | IF expr expr expr { Ast.If { cond = $2; y = $3; n = $4 } }

list_expr:
  | expr { [$1] }
  | expr list_expr { $1 :: $2 }


callcc_expr:
  | CALLCC expr { Ast.Callcc $2 }

let_args: 
  | LBRACKET SYMBOL expr RBRACKET { [($2, $3)] }
  | LBRACKET SYMBOL expr RBRACKET let_args { ($2, $3) :: $5 }

let_expr:
  | LET LPAREN let_args RPAREN expr { Ast.Let { ids = List.map fst $3; defs = List.map snd $3; b = $5 } }

define_expr:
  | DEFINE SYMBOL expr { Ast.Define { name = $2; expr = $3 } }

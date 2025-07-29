%{
    open Ast
%}


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

%start <Ast.expr> s

%%
s: expr EOF { $1 }

expr:
  | literal { $1 }
  | LPAREN list_ RPAREN { $1 }
  | LPAREN lambda RPAREN { $1 }
  | LPAREN if_expr RPAREN { $1 }
  
lambda_args:
  | SYMBOL { [$1] }
  | SYMBOL lambda_args { $1 :: $2 }

lambda:
  | LAMBDA LPAREN lambda_args RPAREN expr { Ast.lambda { ids = $3; body = $4 } }

if_expr:
  | IF expr expr expr { Ast.if_expr { cond = $3; y = $4; n = $5 } }

list_:
  | expr { [$1] }
  | expr list_ { $1 :: $2 }

literal:
  | BOOL { Ast.Bool $1 }
  | NUMBER { Ast.Number $1 }
  | STRING { Ast.String $1 }
  | SYMBOL { Ast.Symbol $1 }


%% (* Footer *)
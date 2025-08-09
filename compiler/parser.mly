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
%token CAR CDR
%token EOF

%start <Compiler_lib.Ast.top_expr list> parse

%{
  open Compiler_lib
  open Ast

  let mk_node: Lexing.position * Lexing.position -> 'a -> 'a node =
  fun ({ pos_fname; pos_lnum; pos_cnum; _ }, _) data ->
    { value = data
    ; id = fresh_node_tag ()
    ; loc = {
        file = pos_fname;
        line = pos_lnum;
        column = pos_cnum;
      } 
    }
%}

%%
parse: exprs EOF { $1 }

exprs:
  | { [] }
  | expr exprs { mk_node $loc (Expr $1) :: $2 }
  | LPAREN define_expr RPAREN exprs { $2 :: $4 }

expr:
  | atom { $1 }
  | LPAREN compound RPAREN { $2 }

atom:
  | BOOL { mk_node $loc (Bool $1) }
  | NUMBER { mk_node $loc (Number $1) }
  | STRING { mk_node $loc (String $1) }
  | SYMBOL { mk_node $loc (Var $1) }
  | list_expr { $1 }

compound:
  | app_expr { mk_node $loc (App $1) }
  | lambda_expr { $1}
  | if_expr { $1 }
  | let_expr { $1 }
  | callcc_expr { $1 }
  | car_expr { $1 }
  | cdr_expr { $1 }

list_expr:
  | QUOTE LPAREN list_elements RPAREN { $3 }

list_elements:
  | { mk_node $loc Nil }
  | expr list_elements { mk_node $loc (Pair ($1, $2)) }

lambda_args:
  | { [] }
  | SYMBOL lambda_args { $1 :: $2 }

lambda_expr:
  | LAMBDA LPAREN lambda_args RPAREN expr { mk_node $loc (Lambda { ids = $3; body = $5 }) }

if_expr:
  | IF expr expr expr { mk_node $loc (If { cond = $2; y = $3; n = $4 }) }

app_expr:
  | expr { [$1] }
  | expr app_expr { $1 :: $2 }

callcc_expr:
  | CALLCC expr { mk_node $loc (Callcc $2) }

let_args: 
  | { [] }
  | LBRACKET SYMBOL expr RBRACKET let_args { ($2, $3) :: $5 }

let_expr:
  | LET LPAREN let_args RPAREN expr { mk_node $loc (Let { defs = $3; body = $5 }) }

define_expr:
  | DEFINE SYMBOL expr { mk_node $loc (Define { name = $2; expr = $3 }) }
  | DEFINE LPAREN SYMBOL lambda_args RPAREN expr { mk_node $loc (Define { name = $3; expr = mk_node $loc (Lambda { ids = $4; body = $6 }) }) }

car_expr:
  | CAR expr { mk_node $loc (Car $2) }

cdr_expr:
  | CDR expr { mk_node $loc (Cdr $2) }

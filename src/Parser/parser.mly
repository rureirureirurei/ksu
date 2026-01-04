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
%token QUOTE
%token EOF

%start <Lang.Ast.top_expr list> parse

%{
  open Lang
  open Ast

  let mk_node: Lexing.position * Lexing.position -> 'a -> 'a node =
  fun ({ pos_fname; pos_lnum; pos_cnum; _ }, _) data ->
    { value = data
    ; loc = {
        file = pos_fname;
        line = pos_lnum;
        column = pos_cnum;
      } 
    }
%}

%%
parse: top_exprs EOF { $1 }

top_exprs:
  | { [] }
  | expr top_exprs { mk_node $loc (E_Expr $1) :: $2 }
  | LPAREN define_expr RPAREN top_exprs { $2 :: $4 }

expr:
  | atom { $1 }
  | LPAREN compound RPAREN { $2 }

atom:
  | BOOL { mk_node $loc (E_Bool $1) }
  | NUMBER { mk_node $loc (E_Number $1) }
  | STRING { mk_node $loc (E_String $1) }
  | SYMBOL { mk_node $loc (E_Var $1) }
  | list_expr { $1 }

compound:
  | lambda_expr { $1}
  | if_expr { $1 }
  | let_expr { $1 }
  | begin_expr { $1 }
  | callcc_expr { $1 }
  | exprs {
      match $1 with
      | f :: args -> mk_node $loc (E_App (f, args))
      | [] -> failwith "Empty application"
    }
  
begin_expr:
  | BEGIN exprs { mk_node $loc (E_Begin $2) }

list_expr:
  | QUOTE LPAREN list_elements RPAREN { $3 }

list_elements:
  | { mk_node $loc E_Nil }
  | expr list_elements { mk_node $loc (E_Pair ($1, $2)) }

lambda_args:
  | { [] }
  | SYMBOL lambda_args { $1 :: $2 }

lambda_expr:
  | LAMBDA LPAREN lambda_args RPAREN expr { mk_node $loc (E_Lambda ($3, $5)) }

if_expr:
  | IF expr expr expr { mk_node $loc (E_If ($2, $3, $4)) }

exprs:
  | expr { [$1] }
  | expr exprs { $1 :: $2 }

callcc_expr:
  | CALLCC expr { mk_node $loc (E_Callcc $2) }

let_args: 
  | { [] }
  | LBRACKET SYMBOL expr RBRACKET let_args { ($2, $3) :: $5 }

let_expr:
  | LET LPAREN let_args RPAREN expr { mk_node $loc (E_Let ($3, $5)) }

define_expr:
  | DEFINE SYMBOL expr { mk_node $loc (E_Define ($2, $3)) }
  | DEFINE LPAREN SYMBOL lambda_args RPAREN expr { mk_node $loc (E_Define ($3, mk_node $loc (E_Lambda ($4, $6)))) }

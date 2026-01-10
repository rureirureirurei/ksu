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

  (* Generate unique symbols for desugaring *)
  let gensym =
    let cnt = ref 0 in
    fun prefix ->
      cnt := !cnt + 1;
      prefix ^ string_of_int !cnt

  (* Get last element of a list *)
  let last lst = List.hd @@ List.rev lst

  (* Desugar begin into let *)
  let desugar_begin exprs =
    match exprs with
    | [] -> E_App (E_Lambda ([], E_Bool true), []) (* empty begin *)
    | [e] -> e (* single expression *)
    | _ ->
        let unused_syms = List.map (fun _ -> gensym "unused_begin_") exprs in
        let body = E_Var (last unused_syms) in
        E_App (E_Lambda (unused_syms, body), exprs)

  (* Desugar let into lambda application *)
  let desugar_let bindings body =
    let syms = List.map fst bindings in
    let exprs = List.map snd bindings in
    E_App (E_Lambda (syms, body), exprs)
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
  | BEGIN exprs { desugar_begin $2 }

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
  | LET LPAREN let_args RPAREN expr { desugar_let $3 $5 }

define_expr:
  | DEFINE SYMBOL expr { E_Define ($2, $3) }
  | DEFINE LPAREN SYMBOL lambda_args RPAREN expr { E_Define ($3, E_Lambda ($4, $6)) }

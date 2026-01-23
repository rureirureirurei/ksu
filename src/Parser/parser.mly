%token <bool> BOOL
%token <int> NUMBER
%token <string> IDENT
%token <string> STRING
%token LPAREN RPAREN
%token LBRACKET RBRACKET
%token DEFINE
%token IF LAMBDA CALLCC
%token COND ELSE
%token LET LET_STAR
%token QUOTE BACKQUOTE COMMA
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
    | [] -> E_App (E_Lambda ([], E_Lit (L_Bool true)), []) (* empty begin *)
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

  (* Desugar let* into nested lambda applications *)
  let desugar_seq_let bindings body =
    List.fold_right (fun (sym, expr) acc ->
      E_App (E_Lambda ([sym], acc), [expr])
    ) bindings body

  let rec exprs2list = function
    | [] -> E_Var "nil"
    | hd :: tail -> E_App (E_Var "pair", [hd; exprs2list tail])


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
  | quote_expr { $1 }
  | quasiquote_expr { $1 }

quote_atom:
  | NUMBER { E_Lit (L_Number $1) }
  | STRING { E_Lit (L_String $1) }
  | BOOL { E_Lit (L_Bool $1) }
  | IDENT { E_Lit (L_Symbol $1) }
  | LAMBDA { E_Lit (L_Symbol "lambda") }
  | IF { E_Lit (L_Symbol "if") }
  | DEFINE { E_Lit (L_Symbol "define") }
  | BEGIN { E_Lit (L_Symbol "begin") }
  | LET { E_Lit (L_Symbol "let") }
  | LET_STAR { E_Lit (L_Symbol "let*") }
  | CALLCC { E_Lit (L_Symbol "call/cc") }
  | LPAREN quote_list RPAREN { exprs2list $2 }
  | LPAREN RPAREN { E_Var "nil" }

quote_list:
  | { [] }
  | quote_atom quote_list { $1 :: $2 }

quote_expr:
  | QUOTE quote_atom { $2 }

quasiquote_atom:
  | NUMBER { E_Lit (L_Number $1) }
  | STRING { E_Lit (L_String $1) }
  | BOOL { E_Lit (L_Bool $1) }
  | IDENT { E_Lit (L_Symbol $1) }
  | LAMBDA { E_Lit (L_Symbol "lambda") }
  | IF { E_Lit (L_Symbol "if") }
  | DEFINE { E_Lit (L_Symbol "define") }
  | BEGIN { E_Lit (L_Symbol "begin") }
  | LET { E_Lit (L_Symbol "let") }
  | LET_STAR { E_Lit (L_Symbol "let*") }
  | CALLCC { E_Lit (L_Symbol "call/cc") }
  | COMMA expr { $2 }
  | LPAREN quasiquote_list RPAREN { exprs2list $2 }
  | LPAREN RPAREN { E_Var "nil" }

quasiquote_list:
  | { [] }
  | quasiquote_atom quasiquote_list { $1 :: $2 }

quasiquote_expr:
  | BACKQUOTE quasiquote_atom { $2 }

atom:
  | BOOL { E_Lit (L_Bool $1) }
  | NUMBER { E_Lit (L_Number $1) }
  | STRING { E_Lit (L_String $1) }
  | IDENT { E_Var $1 }

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
  | cond_expr { $1 }

cond_expr:
  | COND cond_clauses { $2 }

cond_clauses: 
  | LBRACKET ELSE expr RBRACKET { $3 }
  | LBRACKET expr expr RBRACKET cond_clauses { E_If ($2, $3, $5)}
  
begin_expr:
  | BEGIN exprs { desugar_begin $2 }

lambda_args:
  | { [] }
  | IDENT lambda_args { $1 :: $2 }

lambda_expr:
  | LAMBDA LPAREN lambda_args RPAREN expr { E_Lambda ($3, $5) }

if_expr:
  | IF expr expr expr { E_If ($2, $3, $4) }

exprs:
  | expr { [$1] }
  | expr exprs { $1 :: $2 }

callcc_expr:
  | CALLCC IDENT expr { E_Callcc ($2, $3) }

let_args: 
  | { [] }
  | LBRACKET IDENT expr RBRACKET let_args { ($2, $3) :: $5 }

let_expr:
  | LET LPAREN let_args RPAREN expr { desugar_let $3 $5 }
  | LET_STAR LPAREN let_args RPAREN expr { desugar_seq_let $3 $5 }

define_expr:
  | DEFINE IDENT expr { E_Define ($2, $3) }
  | DEFINE LPAREN IDENT lambda_args RPAREN expr { E_Define ($3, E_Lambda ($4, $6)) }

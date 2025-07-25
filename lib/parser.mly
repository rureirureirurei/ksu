(* In the parser, we will define the list of tokens, those will be later accesible via the menhir-generated type *)
%{
    (* Header *)    
%}


(* Literals *)
%token <bool> BOOL_LITERAL
%token <int> INT_LITERAL
%token <string> ID

(* Types *)
%token INT_T
%token BOOL_T
%token ARR (* -> *)

(* Operators - Arithmetic *)
%token PLUS              (** "+"       *)
%token MINUS             (** "-"       *)
%token TIMES             (** "*"       *)
%token DIV               (** "/"       *)
%token MOD               (** "%"       *)

(* Operators - Comparison *)
%token EQUALS            (** "=="      *)
%token NOT_EQUAL         (** "!="      *)
%token LESS              (** "<"       *)
%token GREATER           (** ">"       *)
%token LESS_OR_EQUAL     (** "<="      *)
%token GREATER_OR_EQUAL  (** ">="      *)

(* Operators - Logical *)
%token OR                (** "or"      *)
%token AND               (** "and"     *)
%token NOT               (** "not"     *)

(* Keywords *)
%token LET
%token IN
%token IF
%token THEN
%token ELSE
%token FUN

(* Punctuation *)
%token EQUAL
%token LEFT_PAREN
%token RIGHT_PAREN
%token SEMI (* : *)

(* End of file *)
%token EOF

%start <unit> parse

%% (* Rules *)

parse:
| e EOF {}
| e SEMI parse {}

e:
| LET ID EQUAL e IN e {}
| IF e THEN e ELSE e {}
| FUN ID SEMI type_e ARR e {}
| e binop e {}
| e relop e {}
| e boolop e {}
| unop e {}
| boolunop e {}
| LEFT_PAREN e RIGHT_PAREN {}
| INT_LITERAL {}
| BOOL_LITERAL {}

relop:
| EQUALS {}
| NOT_EQUAL {}
| GREATER {}
| GREATER_OR_EQUAL {}
| LESS {}
| LESS_OR_EQUAL {}

binop:
| PLUS {}
| MINUS {}
| TIMES {}
| DIV {}
| MOD {}

boolop:
| AND {}
| OR {}

unop:
| MINUS {}

boolunop:
| NOT {}

type_e: 
| BOOL_T {}
| INT_T {}
| BOOL_T ARR type_e {}
| INT_T ARR type_e {}
| LEFT_PAREN type_e RIGHT_PAREN ARR type_e {}

%% (* Footer *)
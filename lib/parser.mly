(* In the parser, we will define the list of tokens, those will be later accesible via the menhir-generated type *)

(* Literals *)
%token <bool> BOOL_LITERAL
%token <int> BOOL_LITERAL
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
%token EQUAL             (** "=="      *)
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
%token LEFT_PAREN
%token RIGHT_PAREN
%token SEMI (* : *)

(* End of file *)
%token EOF

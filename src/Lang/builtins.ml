(* Builtin primitives *)

type prim =
  | P_fst
  | P_snd
  | P_pair
  | P_IsNil
  | P_IsPair
  | P_IsList
  | P_IsNumber
  | P_Plus
  | P_Minus
  | P_Mult
  | P_Div
  | P_Eq
  | P_Ne
  | P_Lt
  | P_Le
  | P_Gt
  | P_Ge
  | P_And
  | P_Or
  | P_Not
  | P_Print
  | P_Set
  | P_Box
  | P_Unwrap
  | P_Peek
  | P_StringToSymbol
  | P_Nil
  | P_IsSymbol
  | P_Raise

(* Convert builtin to its Ksu name (for pretty-printing AST) *)
let builtin_to_string = function
  | P_fst -> "fst"
  | P_snd -> "snd"
  | P_pair -> "pair"
  | P_IsNil -> "nil?"
  | P_IsPair -> "pair?"
  | P_IsList -> "list?"
  | P_IsNumber -> "number?"
  | P_Plus -> "+"
  | P_Minus -> "-"
  | P_Mult -> "*"
  | P_Div -> "/"
  | P_Eq -> "="
  | P_Ne -> "!="
  | P_Lt -> "<"
  | P_Le -> "<="
  | P_Gt -> ">"
  | P_Ge -> ">="
  | P_And -> "and"
  | P_Or -> "or"
  | P_Not -> "not"
  | P_Print -> "print"
  | P_Set -> "set!"
  | P_Box -> "box"
  | P_Unwrap -> "unwrap"
  | P_Peek -> "peek"
  | P_Nil -> "nil"
  | P_StringToSymbol -> "string->symbol"
  | P_IsSymbol -> "symbol?"
  | P_Raise -> "raise"


type expr = 
| Bool of bool
| Number of int
| String of string
| Symbol of string
| List of expr list
| Lambda of {ids: string list; b: expr}
| If of { cond: expr; y: expr; n: expr}
| Callcc of expr
| Let of {ids: string list; defs: expr list; b: expr}
| Define of {name: string; expr: expr}


let rec string_of_expr = function
| Bool b -> string_of_bool b
| Number n -> string_of_int n
| String s -> s
| Symbol s -> s
| List l -> "(" ^ String.concat " " (List.map string_of_expr l) ^ ")"
| Lambda {ids; b} -> "(lambda (" ^ String.concat " " ids ^ ") " ^ string_of_expr b ^ ")"
| If {cond; y; n} -> "(if " ^ string_of_expr cond ^ " " ^ string_of_expr y ^ " " ^ string_of_expr n ^ ")"
| Callcc e -> "(callcc " ^ string_of_expr e ^ ")"
| Let {ids; defs; b} -> "(let (" ^ String.concat " " ids ^ ") (" ^ String.concat " " (List.map string_of_expr defs) ^ ") " ^ string_of_expr b ^ ")"
| Define {name; expr} -> "(define " ^ name ^ " " ^ string_of_expr expr ^ ")"
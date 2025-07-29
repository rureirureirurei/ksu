type expr = 
| Bool of bool
| Number of int
| String of string
| Symbol of string
| List of expr list
| Lambda of {ids: string list; e: expr}
| If of { cond: expr; y: expr; n: expr}
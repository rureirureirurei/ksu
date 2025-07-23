type binop = 
| Plus
| Minus

type t = 
| Bool_T
| Int_T
| Fun of t * t

type expr = 
| Bool_Literal of bool
| Int_Literal of int
| Lambda of {id: string; e: expr}
| If of { cond: expr; y: expr; n: expr}
| Let of { id: string; e: expr }
| Op of { op: binop; lhs: expr; rhs: expr }
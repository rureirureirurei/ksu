# ksu

*ksu* is a functional language that supports first class continuations.
It's syntax resembles OCaml.

## Priorities and tasks

- **0** - Core part of the language. Must be implemented as fast as possible.
- **1** - Important functionality that is blocked by the missing core parts. Should be implemented. 
- **2** - Neat idea that can be implemented in a sensible time. 
- **3** - Neat idea that would take substantial amount of time. Most likely will never be implemented.

Things I still have to implement:
  - Specification (0)
    - Types specification (0) 
    - Advanced control flow specification (0)
    - Operators (0)
      - Precenedce (0)
  - Lexer (0)
    - Error reporting (1)
    - Comments (1)
    - Use my own lexer from the FP course (3) 
  - Parser (0)
    - Resolve parsing conflicts (0)
    - Type expressions (0)
    - Clean up the mess between the logical, relational and arithmetic operators (0)
  - Interpreter (0)
    - Fundamental stuff interpreter (0)
    - CPS interpreter for the advanced control flow (0)
  - Types (0)
    - Variant types (2)
    - Record types (2)
  - Pattern matching (3)
  - Arrays (3)

## In general

Program is a single plaintext file that contains sequence of statements. Each statement is either
- Variable binding (0) ```let <id> = <expr>```
- Application (0) ```<expr> <expr>```
- Custom type definition (3) ```type <id> = <type_expr>```

For example

```ksu
let x: int = 10
let y: int = 20
print_int x                  (* prints 20 *)

let f: int -> int = fun n: int -> n * 2 in
let g: int -> int = fun m: int -> m * 3 in 
let k: int = (if x == 10 then 1 else -1) in
    print_int ((f y) + (g x) + k)     (* prints 71 *)

let fact: int -> int = fun n: int -> if n == 0 then 1 else (fact (n - 1)) * n 

print_int (fact 5) (* prints 120*)
```

The program may communicate with the outer world by
- (0) returned value of the last statement
- (1) `print_int: int -> ()`
- (2) `print_string: st`
- (2) `read: () -> string`



## Types

There are 3 main types:
- int
- bool
- 'a -> 'b

There will be no type inference, all variables should have their type specified by the programmer. 

## Keywords and operators 

- let <id> = <expr> in <expr>
- if <expr> then <expr> else <expr>
- fun <id> -> <expr>
- +,-,/,*
- ;

## Examples
```
let x: int = 5 in
let y: int = 10 in
x + y
```

```
let is_positive: int -> bool = fun n: int ->
  if n > 0 then true else false in
is_positive 42
```


```
let max: int -> int -> int =
  fun a: int ->
    fun b: int ->
      if a > b then a else b in
let double: int -> int = fun x: int -> x * 2 in
let result: int = max 10 20 in
double result
```

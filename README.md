# Language specification

The ??? is a functional language that supports first class continuations.
It's syntax resembles OCaml.

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

## TODO
- Lexer
- Parser
- Interpreter
- Interpreter in the CPS
- Playing around with the FCCs a bit more
- Adding advanced control flow operators to the lexer/parser- Implementing them in the interpreter
- Writing AST to IR translation
- Closure conversion
- Register colouring
- IR to ASM translation
- Testing infrastructure

## MAYBE
- Strings
- Modules
- Pattern matching
- Variant types / tuples
- print and read
- Validate int literals 
- Better error reporting in the lexing stage
- Comments

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

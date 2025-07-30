# ksu

*ksu* is a functional language that supports first-class continuations.
Its syntax resembles Scheme.

## Usage

The `ksu` executable can be used in two modes:

### Interpreter Mode (Default)
```bash
ksu program.ksu
```
Runs the program directly by interpreting it. This is the default behavior.

### Compiler Mode
```bash
ksu --compile program.ksu -o output
```
Compiles the program to a native executable. The `-o` flag specifies the output filename.

### Interactive Mode
```bash
ksu
```
Starts an interactive REPL (Read-Eval-Print Loop) where you can enter expressions directly.

## Priorities and tasks

- **0** - Core part of the language. Must be implemented as fast as possible.
- **1** - Important functionality that is blocked by the missing core parts. Should be implemented. 
- **2** - Neat idea that can be implemented in a sensible time. 
- **3** - Neat idea that would take substantial amount of time. Most likely will never be implemented.

Things I still have to implement:
  - Reading
    - Implement userland multithreading (1)
    - Backtracking K elements sum (1)
  - Specification (0)
    - Basic expression specification (2)
    - Continuation specification and examples (0)
    - Built-in functions specification (2)
  - Lexer (0)
    - Error reporting (2)
  - Parser (0)
    - Wrap Ast.expr into the type that will also contain location in file (2)
    - Error reporting (2)
    - Negative numbers (2)
    - Lists (2)
    - Define procedure syntax (2)
  - Interpreter (0)
    - Throw (2)
    - Testing for interpreter (1)
    - Mutations: set! (2)
  - Compiler (0)
    - Types (?)
    - Transform AST: let -> lambda, nested expressions -> let, define -> letrec (?)
    - Validate AST: function arity, special forms valid usage, unbound variables and functions, closure info (0)
    - Closure Conversion: basically remove all free variables by adding env variable (0)
    - CPS transformation (1?)
  - Types (2)
    - Dynamic typing with runtime checks (2)
    - Optional static typing (3)
    - Type inference (3)

## In general

Program is a sequence of S-expressions. Each expression is either:
- **Atom**: number, string, symbol, or boolean
- **Compound**: `(expr expr ...)`

Compound is either special form, or function application.

Special forms:
- `(lambda (params...) body)` - function definition
- `(let ([var val]...) body)` - local bindings  
- `(if test then else)` - conditional
- `(define name value)` - top-level binding
- `(call/cc proc)` - first-class continuations

## Examples

Basic arithmetic and functions:
```scheme
(define x 10)
(define y 20)
(+ x y)  ; returns 30

(define square (lambda (n) (* n n)))
(square 5)  ; returns 25
```

Conditionals and recursion:
```scheme
(define factorial 
  (lambda (n)
    (if (= n 0) 
        1 
        (* n (factorial (- n 1))))))

(factorial 5)  ; returns 120
```

First-class continuations:
```scheme
(+ 1 (call/cc (lambda (k) (+ 2 (k 3)))))  ; returns 4

(define saved-cont #f)
(define result
  (+ 1 (call/cc (lambda (k) 
                  (set! saved-cont k) ; (2) set!
                  10))))
; result is 11, but saved-cont can be called later
(saved-cont 99)  ; jumps back, returns 100
```

## Types

Main types (runtime representation):
- **Number**: integers and floats
- **Boolean**: `#t` and `#f`  
- **String**: `"hello world"`
- **Function**: `(lambda (x) ...)`
- **Continuation**: captured by `call/cc`

## Built-in Functions

Arithmetic: `+`, `-`, `*`, `/`, `=`, `<`, `>`, `<=`, `>=`
I/O: `display`, `newline` (2)
List operations: `car`, `cdr`, `cons`, `null?` (2)
Type predicates: `number?`, `boolean?`, `string?`, `procedure?`, `continuation?`
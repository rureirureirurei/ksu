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
    - Basic expression specification (0)
    - Continuation specification and examples (0)
    - Built-in functions specification (0)
  - Lexer (0)
    - Numbers, strings, symbols, booleans (0)
    - Parentheses and comments (0) 
    - Error reporting (1)
  - Parser (0)
    - S-expression parsing (0)
    - AST construction (0)
  - Interpreter (0)
    - Basic evaluation (lambda, let, if, define) (0)
    - First-class continuations (call/cc) (0)
    - Built-in arithmetic and I/O (1)
  - Types (2)
    - Dynamic typing with runtime checks (2)
    - Optional static typing (3)
    - Type inference (3)
  - Executable (0)
    - Read from stdin (0)
    - Parse arguments (2)
    - Read from file (2)


## In general

Program is a sequence of S-expressions. Each expression is either:
- **Atom**: number, string, symbol, or boolean
- **List**: `(expr expr ...)`

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
                  (set! saved-cont k) 
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
I/O: `display`, `newline`
List operations: `car`, `cdr`, `cons`, `null?`
Type predicates: `number?`, `boolean?`, `string?`, `procedure?`
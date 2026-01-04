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

## TODO
  - Refactor constructor for the App ast node - make function and arguments separate. It would remove stupid App [] checks and make parsing simpler (2)
  - Adequately propagate node locations during closure conversion and flattening (3)

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

### Arithmetic: `+`, `-`, `*`, `/`, `=`, `<`, `>`, `<=`, `>=`
- `+`, `*` - variadic, fold over all arguments
- `-`, `/` - variadic, fold left-to-right starting with first argument  
- `=` - variadic, returns true if all arguments are equal
- `<`, `>`, `<=`, `>=` - binary comparison operators


### Keywords

#### `call/cc`
First-class continuations. Captures the current continuation and passes it to the given procedure. Only lambda expr with with arity 1 can be passed to it.

#### `define`
Top-level binding for variables and functions. Only allowed at the top level.

**Forms:**
- `(define <id> <expr>)` - bind a value to an identifier
- `(define (<id> <arg0> <arg1> ... <argn>) <expr>)` - define a function (equivalent to `(define <id> (lambda (<arg0> <arg1> ... <argn>) <expr>))`)

**Limitations compared to racket:**
- No optional arguments or variable arguments (rest parameters)
- No multiple sequential expressions in the body
- No forward-referencing other defines
- No nested function definitions like `(define ((<name> <arg0>) <arg1>) <expr>)`
- Recursion is allowed (currently only in lambdas)

#### `lambda`
Function definition. Creates a procedure with the given parameters and body. Can have multiple arguments.

#### `let`
Local bindings. Creates a new scope with bound variables. Can have multiple non-recursive definitions.

#### `if`
Conditional expression. Evaluates the test and returns either the then or else expression in the lazy manner.

### `quotes and pairs`

Quotes are not implemented. Cons are, though.
List operations: `car`, `cdr`, `cons` and `null` primitive value are supported.
At the moment, quotes are evaluated as list of exressions (the same as in proper quotes assuming that all values are primitives), and, 
are rather syntactic sugar to cons, than separate mechanism of preserving thing as unevaluated symbols.


### `I/O: `display`, `newline` (2)`
Type predicates: `number?`, `boolean?`, `string?`, `procedure?`, `continuation?`, `null?`, `cons?`

## Liturature 
- https://matt.might.net/articles/closure-conversion/
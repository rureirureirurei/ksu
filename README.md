# Ksu - yet another implementation of LISP

Ksu is a functional dynamically typed language that is compiled to C. Ksu is based on LISP.
Here is an example code:

```ksu
;This one counts factorial

(define factorial 
  (lambda (n)
    (if (= n 0)
        1 
        (* n (factorial (- n 1)))
    )
  )
)

(print (factorial 5))
```

There is support for: 
 - first class functions
 - first class continuations
 - mutable state 
 - self-recursive functions
 - first class builtin operators 
 - pairs
 - quoting

Compiler implements
 - lexing / parsing
 - cps translation (trivial syntactic based approach currently)
 - trampolining (so that cps does not cause stack overflow)

# Installation: 
(todo provide details on dune / utop / ocaml installation) 
Clone the repo and run `./run.sh <.ksu file path>`. Script will also dump debug .ast files in the /tmp.  

# Testing 
`./test/run_test.py`

# Benchmarking 
Procudicing optimal target code was never goal of this project. Nevertheless there is simple benchmarking facility:
`./benchmark/run_benchmarks.py`


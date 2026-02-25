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

Here is an example using `call/cc` for early exit:

```ksu
; Multiply list elements, exit early if zero found
(define (product lst k)
  (if (nil? lst)
      1
      (if (= 0 (fst lst))
          (k 0)
          (* (fst lst) (product (snd lst) k)))))

(print (call/cc k (product '(1 2 3 4 5) k)))   ; 120
(print (call/cc k (product '(1 2 0 4 5) k)))   ; 0 (skips multiplication)
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

# Installation

```bash
# Install OCaml and opam (package manager)
# Fedora/RHEL:
sudo dnf install opam

# Ubuntu/Debian:
# sudo apt install opam

# macOS:
# brew install opam

# Initialize opam and install OCaml
opam init
opam switch create 5.1.0
eval $(opam env)

# Install dependencies
opam install dune menhir
```

Clone the repo and run `./run.sh <.ksu file path>`. Script will also dump debug .ast files in /tmp.  

# Testing 
`./test/run_test.py`

# Benchmarking 
Procudicing optimal target code was never goal of this project. Nevertheless there is simple benchmarking facility:
`./benchmark/run_benchmarks.py`


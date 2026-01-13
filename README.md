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

# Running the project: 
Clone the repo and run `./run.sh <.ksu file path>`. Script will also dump debug .ast files in the /tmp.  

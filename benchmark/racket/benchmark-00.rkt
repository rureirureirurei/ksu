#lang racket
;705082704
(define sum
  (lambda (n)
    (if (= n 0)
        0
        (+ n (sum (- n 1)))
    )
  )
)

(displayln (sum 100000))

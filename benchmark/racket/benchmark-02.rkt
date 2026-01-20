#lang racket
;#t
(define find
  (lambda (needle lst)
    (if (null? lst)
        #f
        (if (= (car lst) needle)
            #t
            (find needle (cdr lst))
        )
    )
  )
)

(define buildlist
  (lambda (n acc)
    (if (= n 0)
        acc
        (buildlist (- n 1) (cons n acc))
    )
  )
)

(define upto
  (lambda (n)
    (buildlist n '())
  )
)

(define n 30000)
(define biglist (upto n))
(displayln (find n biglist))

#lang racket
;12
;Backtracking
(define cont-stack (box '()))

(define (choose options)
  (let/cc k
    (begin
      (set-box! cont-stack (cons (cons k (cdr options)) (unbox cont-stack)))
      (car options)
    )
  )
)

(define cnt (box 0))

(define (fail)
  (if (null? (unbox cont-stack))
      (displayln (unbox cnt))
      (let* ([top (car (unbox cont-stack))] [k (car top)] [options (cdr top)])
        (begin
          (set-box! cont-stack (cdr (unbox cont-stack)))
          (if (null? options)
              (fail)
              (begin
                (set-box! cont-stack (cons (cons k (cdr options)) (unbox cont-stack)))
                (k (car options))
              )
          )
        )
      )
  )
)

(define (range n) (if (= n 0)
  (cons 1 '())
  (cons n (range (- n 1)))
))

(define (triples)
  (let* ([ns (range 23)]
        [a (choose ns)]
        [b (choose ns)]
        [c (choose ns)])
    (begin
      (if (= (+ (* b b) (* c c)) (* a a)) (set-box! cnt (+ 1 (unbox cnt))) '())
      (fail)
    )
  )
)
(define n 100000)
(define a (range n))
(define b (range n))
(define c (range n))

(triples)

#lang racket
;92
; N-Queens: count all solutions for N=8

(define cont-stack (box '()))
(define solution-count (box 0))

(define (choose options)
  (let/cc k
    (begin
      (set-box! cont-stack (cons (cons k (cdr options)) (unbox cont-stack)))
      (car options))))

(define (fail)
  (if (null? (unbox cont-stack))
      (displayln (unbox solution-count))
      (let* ([top (car (unbox cont-stack))]
             [k (car top)]
             [options (cdr top)])
        (begin
          (set-box! cont-stack (cdr (unbox cont-stack)))
          (if (null? options)
              (fail)
              (begin
                (set-box! cont-stack (cons (cons k (cdr options)) (unbox cont-stack)))
                (k (car options))))))))

(define (range-helper n acc)
  (if (= n 0) acc (range-helper (- n 1) (cons n acc))))

(define (range n) (range-helper n '()))

; Check if queen at (row, col) attacks any queen in placed list
(define (attacks? row col placed)
  (if (null? placed)
      #f
      (let* ([p (car placed)]
             [r (car p)]
             [c (cdr p)])
        (if (= c col) #t
            (if (= (abs (- r row)) (abs (- c col))) #t
                (attacks? row col (cdr placed)))))))

(define (solve n row placed)
  (if (> row n)
      (begin
        (set-box! solution-count (+ 1 (unbox solution-count)))
        (fail))
      (let ([col (choose (range n))])
        (if (attacks? row col placed)
            (fail)
            (solve n (+ row 1) (cons (cons row col) placed))))))

(define N 8)
(solve N 1 '())

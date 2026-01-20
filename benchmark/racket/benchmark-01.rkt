#lang racket
;705082705
; Sophisticated nested functions program

(define Y (lambda (f)
           ((lambda (x) (f (lambda (v) ((x x) v))))
            (lambda (x) (f (lambda (v) ((x x) v)))))))

(define factorial (Y (lambda (f)
                        (lambda (n)
                          (if (= n 0)
                              1
                              (+ n (f (- n 1))))))))
(displayln (factorial 100000))

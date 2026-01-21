#lang racket
;#t
; Merge sort benchmark

(define (my-merge xs ys)
  (cond
    [(null? xs) ys]
    [(null? ys) xs]
    [(< (car xs) (car ys))
     (cons (car xs) (my-merge (cdr xs) ys))]
    [else
     (cons (car ys) (my-merge xs (cdr ys)))]))

(define (split-at-helper lst n acc)
  (if (= n 0)
      (cons acc lst)
      (split-at-helper (cdr lst) (- n 1) (cons (car lst) acc))))

(define (my-length lst)
  (if (null? lst) 0 (+ 1 (my-length (cdr lst)))))

(define (mergesort lst)
  (let ([len (my-length lst)])
    (if (< len 2)
        lst
        (let* ([half (quotient len 2)]
               [parts (split-at-helper lst half '())]
               [left (car parts)]
               [right (cdr parts)])
          (my-merge (mergesort left) (mergesort right))))))

; Generate descending list from n to 1
(define (descending n acc)
  (if (= n 0)
      acc
      (descending (- n 1) (cons n acc))))

; Check if list is sorted
(define (sorted? lst)
  (cond
    [(null? lst) #t]
    [(null? (cdr lst)) #t]
    [(<= (car lst) (car (cdr lst)))
     (sorted? (cdr lst))]
    [else #f]))

(define N 10000)
(define lst (descending N '()))
(define sorted-lst (mergesort lst))
(displayln (sorted? sorted-lst))

#lang racket
;200000
; Coroutine ping-pong using scheduler queue

(define queue (box '()))
(define scheduler-k (box #f))
(define counter (box 0))
(define N 100000)

; Queue operations
(define (push-queue k)
  (set-box! queue (cons k (unbox queue))))

(define (pop-queue)
  (if (null? (unbox queue))
      #f
      (let ([result (car (unbox queue))])
        (begin
          (set-box! queue (cdr (unbox queue)))
          result))))

(define (yield)
  (let/cc k
    (begin
      (push-queue k)
      ((unbox scheduler-k) #f))))

(define (quit)
  ((unbox scheduler-k) #f))

(define (scheduler)
  (begin
    (let/cc k
      (set-box! scheduler-k k))
    (let ([next (pop-queue)])
      (if next
          (next #f)
          #f))))

(define (ping n)
  (if (= n 0)
      (quit)
      (begin
        (set-box! counter (+ 1 (unbox counter)))
        (yield)
        (ping (- n 1)))))

(define (pong n)
  (if (= n 0)
      (quit)
      (begin
        (set-box! counter (+ 1 (unbox counter)))
        (yield)
        (pong (- n 1)))))

; Spawn both coroutines
(push-queue (lambda (x) (ping N)))
(push-queue (lambda (x) (pong N)))

; Run scheduler
(void (scheduler))

(displayln (unbox counter))

#lang racket

(require rackunit)

(define (make-accumulator n)
  (lambda (x) (begin
               (set! n (+ n x))
               n
               ))
  )

(define subject (make-accumulator 100))

(check-equal? (subject 10) 110)
(check-equal? (subject -120) -10)

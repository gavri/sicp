#lang racket

(define (f x)
  (define (even? n)
    (if (= n 0)
      true
      (odd? (- n 1))))
  (define (odd? n)
    (if (= n 0)
      false
      (even? (- n 1))))
  (even? x))

(require rackunit)

(check-true (f 0))
(check-false (f 1))
(check-false (f 9))
(check-true (f 10))
(check-false (f 11))

#lang racket

(define (f x)
  ((lambda (even? odd?)
     (even? even? odd? x))
   (lambda (ev? od? n)
     (if (= n 0) true (od? ev? od? (- n 1))))
   (lambda (ev? od? n)
     (if (= n 0) false (ev? ev? od? (- n 1))))))

(require rackunit)

(check-true (f 0))
(check-false (f 1))
(check-false (f 9))
(check-true (f 10))
(check-false (f 11))

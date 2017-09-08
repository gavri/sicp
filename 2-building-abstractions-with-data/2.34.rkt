#lang racket
(require srfi/1)
(define (horner-eval x coefficient-sequence)
  (reduce (lambda (this-coeff higher-terms) (+ (* higher-terms x) this-coeff))
          0
          coefficient-sequence))

(require rackunit)
(check-equal? (horner-eval 2 (list 1 3 0 5 0 1)) 1)

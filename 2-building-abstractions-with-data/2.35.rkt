#lang racket
(require srfi/1)

(define (count-leaves x)
  (fold + 0 (map
              (lambda (x) (cond ((list? x) (count-leaves x)) (else 1)))
              x)))

(require rackunit)
(check-equal? (count-leaves '()) 0)
(check-equal? (count-leaves '(7)) 1)
(check-equal? (count-leaves '(() (2) (2 2) (2 (2 2) 2))) 7)

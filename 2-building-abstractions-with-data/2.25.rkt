#lang racket
(require rackunit)
(define first-input '(1 3 (5 7) 9))
(define second-input '((7)))
(define third-input '(1 (2 (3 (4 (5 (6 7)))))))
(define first-selector (compose car cdr car cdr cdr))
(define second-selector (compose car car))
(define third-selector (compose car cdr car cdr car cdr car cdr car cdr car cdr))
(check-equal? (first-selector first-input) 7)
(check-equal? (second-selector second-input) 7)
(check-equal? (third-selector third-input) 7)


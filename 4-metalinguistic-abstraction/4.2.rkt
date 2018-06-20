#lang racket

(define (application? exp) (eq? (car exp) 'call))
(define (operator exp) (cadr exp))
(define (operands exp) (cddr exp))

(require rackunit)

(check-true (application? '(call + 1 2)))
(check-false (application? '(+ 1 2)))
(check-eq? (operator '(call + 1 2)) '+)
(check-equal? (operands '(call + 1 2)) '(1 2))

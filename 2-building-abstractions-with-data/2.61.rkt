#lang racket

(define (adjoin-set x set)
  (cond ((null? set) (list x))
        ((< x (car set)) (cons x set))
        ((eq? x (car set)) set)
        (else (cons (car set) (adjoin-set x (cdr set))))))

(require rackunit)

(check-equal? (adjoin-set 3 '(1 2 4 5)) '(1 2 3 4 5))

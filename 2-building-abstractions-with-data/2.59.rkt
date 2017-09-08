#lang racket

(define (element-of-set? x set)
  (cond ((null? set) false)
        ((equal? x (car set)) true)
        (else (element-of-set? x (cdr set)))))

(define (union-of-set set1 set2)
  (cond ((null? set1) set2)
        ((element-of-set? (car set1) set2) (union-of-set (cdr set1) set2))
        (else (union-of-set (cdr set1) (cons (car set1) set2)))))

(require rackunit)

(check-equal? (union-of-set '(1 2 3) '(3 4 5)) '(2 1 3 4 5))

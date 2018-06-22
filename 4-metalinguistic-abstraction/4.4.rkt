#lang racket

(define (true? x) (not (eq? x 'false)))

(define (and? exp) (and (pair? exp) (eq? (car exp) 'and)))
(define (or? exp) (and (pair? exp) (eq? (car exp) 'or)))

(define (eval-and exp env)
  (cond ((null? exp) true)
        ((null? (cdr exp)) (car exp))
    ((my-eval (true? (car exp)) env) (eval-and (cdr exp) env))
    (else false)))

(define (eval-or exp env)
  (cond ((null? exp) false)
    ((my-eval (true? (car exp)) env) (my-eval (car exp) env))
    (else (eval-or (cdr exp) env))))

(define (my-eval exp env)
  (cond ((and? exp) (eval-and (cdr exp) env))
        ((or? exp) (eval-or (cdr exp) env))
        (else exp)))

(require rackunit)

(check-equal? (my-eval '(and 1 2 false 3) '()) false)
(check-equal? (my-eval '(and 1 2 3) '()) 3)
(check-equal? (my-eval '(and) '()) true)

(check-equal? (my-eval '(or false 1 false 2) '()) 1)
(check-equal? (my-eval '(or false false) '()) false)
(check-equal? (my-eval '(or) '()) false)

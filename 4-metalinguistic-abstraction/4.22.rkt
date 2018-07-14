#lang racket

(define (let? exp) (and (pair? exp) (eq? (car exp) 'let)))
(define (let-var-exps exp) (cadr exp))
(define (let-vars exp) (map car (let-var-exps exp)))
(define (let-exps exp) (map cadr (let-var-exps exp)))
(define (let-body exp) (cddr exp))

(define (let->combination exp)
  (cons (cons 'lambda (cons (let-vars exp) (let-body exp))) (let-exps exp))
  )

(define (analyze exp)
  (cond ((let? exp) (let->combination exp))
        (else exp)))

(require rackunit)

(check-equal? (analyze 1) 1)
(check-equal? (analyze '(let ((x 1) (y 2)) (println "entered") (+ x y))) '((lambda (x y) (println "entered") (+ x y)) 1 2))

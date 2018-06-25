#lang racket

(define (let-var-exps exp) (cadr exp))
(define (let-vars exp) (map car (let-var-exps exp)))
(define (let-exps exp) (map cadr (let-var-exps exp)))
(define (let-body exp) (cddr exp))

(define (let->combination exp)
  (cons (cons 'lambda (cons (let-vars exp) (let-body exp))) (let-exps exp))
  )

(define (eval exp) "no")

(require rackunit)
(check-equal? (let->combination '(let ((x 1) (y 2)) (println "entered") (+ x y))) '((lambda (x y) (println "entered") (+ x y)) 1 2))

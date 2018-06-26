#lang racket

(define (let*->nested-lets exp)
  (let*->nested-lets-recur (let*-var-exps exp) (let*-body exp)))

(define (let*->nested-lets-recur var-exps body)
  (if (null? var-exps)
    body
    (list 'let (list (car var-exps)) (let*->nested-lets-recur (cdr var-exps) body))))

(define (begin-clause exp) (cons 'begin exp))

(define (let*-var-exps exp) (cadr exp))
(define (let*-body exp) (begin-clause (cddr exp)))

(define (eval exp) "no")

(require rackunit)
(check-equal? (let*->nested-lets '(let* ((x 3) (y (+ x 2)) (z (+ x y 5))) (* x z))) '(let ((x 3)) (let ((y (+ x 2))) (let ((z (+ x y 5))) (begin (* x z))))))
(check-equal? (let*->nested-lets '(let* ((x 3) (y (+ x 2)) (z (+ x y 5))) (println "testing body with multiple expressions") (* x z))) '(let ((x 3)) (let ((y (+ x 2))) (let ((z (+ x y 5))) (begin (println "testing body with multiple expressions") (* x z))))))

#lang racket

(define (named-let? full-exp) (symbol? (cadr full-exp)))
(define (let-var-exps exp) (car exp))
(define (let-vars exp) (map car (let-var-exps exp)))
(define (let-exps exp) (map cadr (let-var-exps exp)))
(define (let-body exp) (cdr exp))

(define (let->combination full-exp)
  (if (named-let? full-exp)
    (let ((exp (cddr full-exp)) (name (cadr full-exp)))
      (cons 'begin (list (cons 'define (cons (cons name (let-vars exp)) (let-body exp))) (cons name (let-exps exp)))))
    (let ((exp (cdr full-exp)))
      (cons (cons 'lambda (cons (let-vars exp) (let-body exp))) (let-exps exp)))))

(require rackunit)
(check-equal? (let->combination '(let ((x 1) (y 2)) (println "entered") (+ x y))) '((lambda (x y) (println "entered") (+ x y)) 1 2))
(check-equal? (let->combination '(let fib-iter ((a 1) (b 0) (count n)) (if (= count 0) b (fib-iter (+ a b) a (- count 1))))) '(begin (define (fib-iter a b count) (if (= count 0) b (fib-iter (+ a b) a (- count 1)))) (fib-iter 1 0 n)))

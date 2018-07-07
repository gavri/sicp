#lang racket

(define (definition? exp) (cond ((not (pair? exp)) false)
                                ((eq? (car exp) 'define) true)
                                (else false)))

(define (params-from-proc proc) (cadr proc))
(define (body proc) (cddr proc))
(define (definitions-from-body body) (filter definition? body))
(define (body-without-definitions body) (filter (compose not definition?) body))

(define (scan-out-defines proc)
  (let* ((body (body proc))
        (definitions (definitions-from-body body))
        (definition-vars (map cadr definitions))
        (rest-of-body (body-without-definitions body)))
        (list 'lambda (params-from-proc proc)
              (cons 'let
                    (cons
                      (map (lambda (definition-var) (list definition-var ''*unassigned*)) definition-vars)
                      (append
                        (map (lambda (definition) (list 'set! (second definition) (third definition))) definitions)
                        (body-without-definitions body)))))))


(require rackunit)

(define input '(lambda (vars) (define u e1) (define v e2) e3 (define w e4) e5))
(define expected '(lambda (vars) (let ((u (quote *unassigned*)) (v (quote *unassigned*)) (w (quote *unassigned*))) (set! u e1) (set! v e2) (set! w e4) e3 e5)))

(check-equal? (scan-out-defines input) expected)

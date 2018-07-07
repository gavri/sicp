#lang racket

(define (definition? exp) (cond ((not (pair? exp)) false)
                                ((eq? (car exp) 'define) true)
                                (else false)))

(define (body proc) (cddr proc))
(define (definitions-from-body body) (filter definition? body))
(define (body-without-definitions body) (filter (compose not definition?) body))


(define (has-definitions? body)
  (memf (lambda (exp) (and (pair? exp) (eq? (car exp) 'define))) body))

(define (scan-out-defines-from-body body)
  (let* ((definitions (definitions-from-body body))
         (definition-vars (map cadr definitions))
         (rest-of-body (body-without-definitions body)))
    (cond ((not (has-definitions? body)) body)
          (else (list
                  (cons 'let
                        (cons
                          (map (lambda (definition-var) (list definition-var ''*unassigned*)) definition-vars)
                          (append
                            (map (lambda (definition) (list 'set! (second definition) (third definition))) definitions)
                            (body-without-definitions body)))))))))

(define (scan-out-defines proc)
  (cons (car proc) (cons (cadr proc) (scan-out-defines-from-body (cddr proc)))))

(define (make-procedure parameters body)
  (cond ((has-definitions? body) (cons 'procedure (cons parameters (scan-out-defines-from-body body))))
                                (else (list 'procedure parameters body))))

(require rackunit)

(define anon-proc-input '(lambda (vars) (define u e1) (define v e2) e3 (define w e4) e5))
(define anon-proc-expected '(lambda (vars) (let ((u (quote *unassigned*)) (v (quote *unassigned*)) (w (quote *unassigned*))) (set! u e1) (set! v e2) (set! w e4) e3 e5)))
(check-equal? (scan-out-defines anon-proc-input) anon-proc-expected)

(define named-proc-input '(define (vars) (define u e1) (define v e2) e3 (define w e4) e5))
(define named-proc-expected '(define (vars) (let ((u (quote *unassigned*)) (v (quote *unassigned*)) (w (quote *unassigned*))) (set! u e1) (set! v e2) (set! w e4) e3 e5)))
(check-equal? (scan-out-defines named-proc-input) named-proc-expected)

(define without-internal-definitions '(lambda (vars) e1 e2))
(check-equal? (scan-out-defines without-internal-definitions) without-internal-definitions)

(check-equal?
  (make-procedure '(a b c) '((define a 1) (define b 2) e1 e2))
  '(procedure (a b c) (let ((a (quote *unassigned*)) (b (quote *unassigned*))) (set! a 1) (set! b 2) e1 e2))
  )

(check-equal?
  (make-procedure '(a b c) '(e1 e2))
  '(procedure (a b c) (e1 e2))
  )

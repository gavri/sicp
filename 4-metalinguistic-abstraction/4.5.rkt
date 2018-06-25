#lang racket

(define (if? exp) (and (pair? exp) (eq? (car exp) 'if)))

(define (eval-sequence exps)
  (cond ((last-exp? exps) (eval (first-exp exps)))
        (else (eval (first-exp exps))
              (eval-sequence (rest-exps exps)))))

(define (make-if predicate consequent alternative)
  (list 'if predicate consequent alternative))

(define (begin? exp) (tagged-list? exp 'begin))
(define (begin-actions exp) (cdr exp))
(define (last-exp? seq) (null? (cdr seq)))
(define (first-exp seq) (car seq))
(define (rest-exps seq) (cdr seq))

(define (sequence->exp seq)
  (cond ((null? seq) seq)
        ((last-exp? seq) (first-exp seq))
        (else (make-begin seq))))
(define (make-begin seq) (cons 'begin seq))

(define (tagged-list? exp tag)
  (if (pair? exp)
    (eq? (car exp) tag)
    false))

(define (cond? exp) (tagged-list? exp 'cond))
(define (cond-clauses exp) (cdr exp))
(define (cond-else-clause? clause)
  (eq? (cond-predicate clause) 'else))
(define (cond-predicate clause) (car clause))
(define (cond-actions clause) (cdr clause))
(define (test-recepient-clause? clause) (eq? (cadr clause) '=>))
(define (test-recepient-action clause) (caddr clause))
(define (cond->if exp)
  (expand-clauses (cond-clauses exp)))
(define (expand-clauses clauses)
  (if (null? clauses)
    'false
    ; no else clause
    (let ((first (car clauses))
          (rest (cdr clauses)))
      (if (cond-else-clause? first)
        (if (null? rest)
          (sequence->exp (cond-actions first))
          (error "ELSE clause isn't last -- COND->IF"
                 clauses))
        (if (test-recepient-clause? first)
          (let ((pred (cond-predicate first)))
            (make-if pred (list (test-recepient-action first) pred) (expand-clauses rest)))
          (make-if (cond-predicate first)(sequence->exp (cond-actions first))
                   (expand-clauses rest)))))))

(define (true? x) (not (eq? x 'false)))
(define (if-predicate exp) (cadr exp))
(define (if-consequent exp) (caddr exp))
(define (if-alternative exp)
  (if (not (null? (cdddr exp)))
    (cadddr exp)
    'false))

(define (eval-if exp)
  (if (true? (if-predicate exp))
    (eval (if-consequent exp))
    (eval (if-alternative exp))))

(define (eval exp)
  (cond ((if? exp) (eval-if exp))
        ((begin? exp)
         (eval-sequence (begin-actions exp)))
        ((cond? exp) (eval (cond->if exp)))
        ((equal? exp '(cadr "arg")) "recepient called with test result")
        (else exp)))

(require rackunit)

(check-equal? (eval '(cond (true 1) (else 2))) 1)
(check-equal? (eval '(cond (false 1) (else 2))) 2)
(check-equal? (eval '(cond ("arg" => cadr) (else false))) "recepient called with test result")

#lang racket

(define (letrec-varvals exp) (cadr exp))
(define (letrec-vars exp) (map car (letrec-varvals exp)))
(define (letrec-vals exp) (map cadr (letrec-varvals exp)))
(define (letrec-body exp) (cddr exp))

(define (letrec-vars->let-unassigned-vars vars)
  (map (lambda (var) (list var ''*unassigned*)) vars))

(define (letrec-varvals->let-set-vars varvals)
  (map (lambda (varval) (list 'set! (car varval) (cadr varval))) varvals))

(define (letrec->let exp)
  (cons 'let (cons
               (letrec-vars->let-unassigned-vars (letrec-vars exp))
               (append (letrec-varvals->let-set-vars (letrec-varvals exp)) (letrec-body exp)))))

(require rackunit)

(define input (quote (letrec ((a (lambda () b)) (b 10)) (+ (a) b))))
(define expected (quote (let ((a '*unassigned*) (b '*unassigned*)) (set! a (lambda () b)) (set! b 10) (+ (a) b))))

(check-equal? (letrec->let input) expected)

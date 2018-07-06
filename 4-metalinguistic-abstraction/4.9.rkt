#lang racket

(define (exp-pred exp) (cadr exp))
(define (exp-body exp) (cddr exp))

(define (while->recursive exp)
  (let ((pred (exp-pred exp))
        (body (exp-body exp)))
    (list 'begin (list 'define '(control-loop) (list 'if pred (cons 'begin (append body (list '(control-loop)))) ''())) '(control-loop))))

(define (until->recursive exp)
  (let ((pred (exp-pred exp))
        (body (exp-body exp)))
    (list 'begin (list 'define '(control-loop) (list 'if pred ''() (cons 'begin (append body (list '(control-loop)))))) '(control-loop))))

(require rackunit)
(check-equal?
  (while->recursive
    '(while (pred-arg) (exp-1) (exp-2))
    )
  '(begin (define (control-loop) (if (pred-arg) (begin (exp-1) (exp-2) (control-loop)) (quote ()))) (control-loop))
  )
(check-equal?
  (until->recursive
    '(until (pred-arg) (exp-1) (exp-2))
    )
  '(begin (define (control-loop) (if (pred-arg) (quote ()) (begin (exp-1) (exp-2) (control-loop)))) (control-loop))
  )

#lang racket

(define first-operand car)
(define rest-operands cdr)
(define no-operands? empty?)

(define (list-of-values-ltr exps env)
  (if (no-operands? exps)
    '()
    (cons (eval (first-operand exps) env)
          (list-of-values-ltr (rest-operands exps) env))))

(define (list-of-values-rtl exps env)
  (if (no-operands? exps)
    '()
    (let ((tail (list-of-values-rtl (rest-operands exps) env))
      (head (eval (first-operand exps) env)))
    (cons head tail))))

(require rackunit)
(require compatibility/mlist)

(define ltr-result (mlist 'ltr))
(define rtl-result (mlist 'rtl))

(define (eval x env) (mappend! env (mlist x)) x)

(check-equal? (list-of-values-ltr '(1 2 3) ltr-result) '(1 2 3))
(check-equal? ltr-result (mlist 'ltr 1 2 3))

(check-equal? (list-of-values-rtl '(1 2 3) rtl-result) '(1 2 3))
(check-equal? rtl-result (mlist 'rtl 3 2 1))

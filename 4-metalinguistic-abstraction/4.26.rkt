#lang racket

(define (unless-pred exp) (cadr exp))
(define (unless-antecedent exp) (caddr exp))
(define (unless-consequent exp) (cadddr exp))

(define (unless->if exp)
  (list 'if (unless-pred exp) (unless-consequent exp) (unless-antecedent exp)))

(require rackunit)

(check-equal? (unless->if '(unless (((pred))) (((antecedent))) (((consequent))))) '(if (((pred))) (((consequent))) (((antecedent)))))

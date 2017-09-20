#lang racket

(require rackunit)

(define (make-account balance password)
  (define (withdraw amount)
    (if (>= balance amount)
      (begin (set! balance (- balance amount))
             balance)
      "Insufficient funds"))
  (define (deposit amount)
    (set! balance (+ balance amount))
    balance)
  (define (dispatch candidate-password m)
    (if (eq? password candidate-password)
      (cond ((eq? m 'withdraw) withdraw)
            ((eq? m 'deposit) deposit)
            (else (error "Unknown request -- MAKE-ACCOUNT"
                         m)))
      (error "Incorrect password")))
  dispatch)

(define subject (make-account 100 'secret))

(check-equal? ((subject 'secret 'withdraw) 10) 90)
(check-equal? ((subject 'secret 'deposit) 20) 110)
(check-exn exn:fail? (lambda () ((subject 'wrong-secret 'deposit) 20)))
(check-not-exn (lambda () ((subject 'secret 'deposit) 20)))

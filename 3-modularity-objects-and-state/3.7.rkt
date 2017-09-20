#lang racket

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

(define (make-joint original-account original-account-password new-account-password)
  (define (dispatch candidate-password m)
    (if (eq? new-account-password candidate-password)
      (lambda (arg) ((original-account original-account-password m) arg))
      (error "Incorrect password")))
  dispatch)

(define paul-account (make-account 100 'pauls-secret))
(define peter-account (make-joint paul-account 'pauls-secret 'peters-secret))

(require rackunit)

(check-equal? ((paul-account 'pauls-secret 'withdraw) 10) 90)
(check-equal? ((peter-account 'peters-secret 'deposit) 20) 110)
(check-exn exn:fail? (lambda () ((peter-account 'pauls-secret 'deposit) 20)))

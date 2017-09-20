#lang racket

(require rackunit)

(define (call-the-cops)
  "Calling the cops")

(define (make-account balance password)
  (let ((number-of-consecutive-wrong-passwords 0))
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
      (begin
        (set! number-of-consecutive-wrong-passwords 0)
        (cond ((eq? m 'withdraw) withdraw)
              ((eq? m 'deposit) deposit)
              (else (error "Unknown request -- MAKE-ACCOUNT"
                           m))))
      (begin
        (set! number-of-consecutive-wrong-passwords (+ number-of-consecutive-wrong-passwords 1))
        (if (eq? number-of-consecutive-wrong-passwords 7)
          (call-the-cops)
          (error "Incorrect password"))
        )))
  dispatch))

(define subject (make-account 100 'secret))

(check-equal? ((subject 'secret 'withdraw) 10) 90)

(check-exn exn:fail? (lambda () (subject 'wrong-secret 'deposit)))
(check-exn exn:fail? (lambda () (subject 'wrong-secret 'deposit)))
(check-exn exn:fail? (lambda () (subject 'wrong-secret 'deposit)))
(check-exn exn:fail? (lambda () (subject 'wrong-secret 'deposit)))
(check-exn exn:fail? (lambda () (subject 'wrong-secret 'deposit)))
(check-exn exn:fail? (lambda () (subject 'wrong-secret 'deposit)))

(check-equal? ((subject 'secret 'deposit) 20) 110)

(check-exn exn:fail? (lambda () (subject 'wrong-secret 'deposit)))
(check-exn exn:fail? (lambda () (subject 'wrong-secret 'deposit)))
(check-exn exn:fail? (lambda () (subject 'wrong-secret 'deposit)))
(check-exn exn:fail? (lambda () (subject 'wrong-secret 'deposit)))
(check-exn exn:fail? (lambda () (subject 'wrong-secret 'deposit)))
(check-exn exn:fail? (lambda () (subject 'wrong-secret 'deposit)))
(check-eq? (subject 'wrong-secret 'deposit) "Calling the cops")

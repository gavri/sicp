#lang racket
(require rackunit)
(define (filter p list)
  (cond
    [(null? list) '()]
    [(p (car list)) (cons (car list) (filter p (cdr list)))]
    [else (filter p (cdr list))]
  )
  )
(define (same-parity reference . list)
  (define the-mod (modulo reference 2))
  (define (has-same-parity x) (eq? (modulo x 2) the-mod))
  (cons reference (filter has-same-parity list))
)
(check-equal? (same-parity 1 2 3 4 5 6 7) '(1 3 5 7))
(check-equal? (same-parity 2 3 4 5 6 7) '(2 4 6))
(check-equal? (same-parity 1) '(1))
(check-equal? (same-parity 1 2) '(1))

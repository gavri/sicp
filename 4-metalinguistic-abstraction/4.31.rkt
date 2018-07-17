#lang racket

(require rackunit)

(define logs '())

(define (f a (b lazy) c (d lazy-memo))
  (log 'start)
  a b c d
  (log 'mid)
  a b c d
  (log 'end))

(define (log x)
  (set! logs (cons x logs)) x)

(f (log 'applicative-one) (log 'lazy) (log 'applicative-two) (log 'lazy-memo))

(check-equal? logs '(applicative-one applicative-two start lazy lazy-memo mid lazy end))

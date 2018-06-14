#lang racket

(require rackunit)
(require srfi/41)

(define sense-data (stream -10 20 -10 0 5 -15 -10 -20 5))

(define (sign-change-detector curr prev)
  (cond ((or (and (>= curr 0) (>= prev 0)) (and (< curr 0) (< prev 0))) 0)
        ((>= curr 0) 1)
        (else -1)))

(define zero-crossings
  (stream-map sign-change-detector sense-data (stream-cons 0 sense-data)))

(check-equal? (stream->list zero-crossings) '(-1 1 -1 1 0 -1 0 0 1))

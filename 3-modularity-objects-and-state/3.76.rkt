#lang racket

(require rackunit)
(require srfi/41)

(define (mean x y) (/ (+ x y) 2))

(define (sign-change-detector curr prev)
  (cond ((or (and (>= curr 0) (>= prev 0)) (and (< curr 0) (< prev 0))) 0)
        ((>= curr 0) 1)
        (else -1)))

(define sense-data (stream* 5 -2 10 5 -10 -2 4 0 10 -20 -5 -10 20 -200 100 sense-data))

(define (smooth s) (smooth-recur s 0))

(define (smooth-recur s prev)
  (stream-cons (mean (stream-car s) prev) (smooth-recur (stream-cdr s) (stream-car s)))
  )

(define zero-crossings-without-smoothing
  (stream-map sign-change-detector sense-data (stream-cons 0 sense-data)))

(define zero-crossings
  (stream-map sign-change-detector (smooth sense-data) (stream-cons 0 (smooth sense-data))))

(define (take-to-list n s) (stream->list (stream-take n s)))
(check-equal? (take-to-list 15 (smooth sense-data)) '(5/2 3/2 4 15/2 -5/2 -6 1 2 5 -5 -25/2 -15/2 5 -90 -50))
(check-equal? (take-to-list 15 zero-crossings-without-smoothing) '(0 -1 1 0 -1 0 1 0 0 -1 0 0 1 -1 1))
(check-equal? (take-to-list 15 zero-crossings) '(0 0 0 0 -1 0 1 0 0 -1 0 0 1 -1 0))

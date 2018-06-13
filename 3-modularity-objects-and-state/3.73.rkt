#lang racket

(define (scale-stream stream factor)
  (stream-map (lambda (x) (* x factor)) stream))

(define (add-streams s1 s2)
  (stream-map + s1 s2))

(define (integral integrand initial-value dt)
  (define int
    (stream-cons initial-value
                 (add-streams (scale-stream integrand dt)
                              int)))
  int)

(define (RC r c time-step)
  (lambda (i v0)
    (add-streams (scale-stream i r) (integral (scale-stream i (/ 1 c)) v0 time-step))
  ))


(define RC1 (RC 5 1 0.5))

(require rackunit)
(require srfi/41)

(define natural-numbers (stream-cons 1 (stream-map (lambda (x) (+ x 1)) natural-numbers)))

(define (take-10 n s) (stream->list (stream-take n s)))
(check-equal? (take-10 10 (RC1 natural-numbers 10)) '(15 20.5 26.5 33.0 40.0 47.5 55.5 64.0 73.0 82.5))

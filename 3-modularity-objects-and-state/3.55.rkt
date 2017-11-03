#lang racket

(define (partial-sums-recur s acc)
  (let ((curr (+ (stream-first s) acc)))
  (stream-cons curr (partial-sums-recur (stream-rest s) curr))))

(define (partial-sums s)
  (partial-sums-recur s 0))

(require rackunit)
(require srfi/41)

(define (integers-starting-from n)
  (stream-cons n (integers-starting-from (+ n 1))))

(define integers (integers-starting-from 1))

(check-equal? (stream->list (stream-take 5 (partial-sums integers))) '(1 3 6 10 15))



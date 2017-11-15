#lang racket

(define (stream-map-multi proc argstreams)
  (if (stream-empty? (car argstreams))
    empty-stream
    (stream-cons
      (apply proc (map stream-first argstreams))
      (apply stream-map-multi
             (list proc (map stream-rest argstreams))))))

(define (integers-starting-from n)
    (stream-cons n (integers-starting-from (+ n 1))))

(define integers (integers-starting-from 1))

(define (reciprocals-of s)
  (stream-map (lambda (x) (/ 1 x)) s))

(define (integrate-series input)
  (stream-map-multi * (list input (reciprocals-of integers))))

(define exp-series
  (stream-cons 1 (integrate-series exp-series)))


(define (stream-negate s) (stream-map - s))

(define sine-series (stream-cons 0 (integrate-series cosine-series)))
(define cosine-series (stream-cons 1 (integrate-series (stream-negate sine-series))))

(require rackunit)
(require srfi/41)

(check-equal? (stream->list (stream-take 10 exp-series)) '(1 1 1/2 1/6 1/24 1/120 1/720 1/5040 1/40320 1/362880))
(check-equal? (stream->list (stream-take 10 sine-series)) '(0 1 0 -1/6 0 1/120 0 -1/5040 0 1/362880))
(check-equal? (stream->list (stream-take 10 cosine-series)) '(1 0 -1/2 0 1/24 0 -1/720 0 1/40320 0))

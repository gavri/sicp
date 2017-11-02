#lang racket

(define (integers-starting-from n)
  (stream-cons n (integers-starting-from (+ n 1))))

(define (mul-streams first-stream second-stream)
  (cond ((stream-empty? first-stream) empty-stream)
        (else (stream-cons (* (stream-first first-stream) (stream-first second-stream))
               (mul-streams (stream-rest first-stream) (stream-rest second-stream))))))

(define factorials (stream-cons 1 (mul-streams (integers-starting-from 2) factorials)))

(require rackunit)
(require srfi/41)

(check-equal? (stream->list (stream-take 10 factorials)) '(1 2 6 24 120 720 5040 40320 362880 3628800))

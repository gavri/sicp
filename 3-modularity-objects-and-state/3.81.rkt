#lang racket

(define (rand-update prev)
  (modulo (* 7 prev) 1000))

(define (random-numbers seed)
  (define rands
    (stream-cons seed (stream-map rand-update rands)))
  rands)

(define (random-number-generator-recur request-stream random-numbers-stream)
  (if (stream-null? request-stream) empty-stream
    (if (eq? (stream-car request-stream) 'generate)
      (stream-cons (stream-car random-numbers-stream) (random-number-generator-recur (stream-cdr request-stream) (stream-cdr random-numbers-stream)))
      (random-number-generator-recur (stream-cdr request-stream) (random-numbers (stream-car request-stream))))))

(define (random-number-generator request-stream)
  (random-number-generator-recur request-stream (random-numbers 42)))

(require rackunit)
(require srfi/41)

(define (take-15 s) (stream->list (stream-take 15 s)))

(define first-input (stream 'generate 'generate 100 'generate 'generate 'generate 20 'generate 'generate 'generate 100 'generate 'generate 'generate 'generate))
(check-equal? (take-15 (random-number-generator first-input)) '(42 294 100 700 900 20 140 980 100 700 900 300))

(define second-input (stream 50 'generate 'generate))
(check-equal? (take-15 (random-number-generator second-input)) '(50 350))

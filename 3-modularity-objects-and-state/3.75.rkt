#lang racket

(require rackunit)
(require srfi/41)

(define (mean x y) (/ (+ x y) 2))

(define (sign-change-detector curr prev)
  (cond ((or (and (>= curr 0) (>= prev 0)) (and (< curr 0) (< prev 0))) 0)
        ((>= curr 0) 1)
        (else -1)))

(define sense-data (stream* 5 -2 10 5 -10 -2 4 0 10 -20 -5 -10 20 -200 100 sense-data))

(define (make-zero-crossings input-stream smoothed-last-value last-value)
  (let ((avpt (mean (stream-car input-stream) last-value)))
    (stream-cons (sign-change-detector avpt smoothed-last-value) (make-zero-crossings (stream-cdr input-stream)
                                                                            avpt (stream-car input-stream)))))
(define zero-crossings (make-zero-crossings sense-data 0 0))

(define (take-to-list n s) (stream->list (stream-take n s)))
(check-equal? (take-to-list 15 zero-crossings) '(0 0 0 0 -1 0 1 0 0 -1 0 0 1 -1 0))

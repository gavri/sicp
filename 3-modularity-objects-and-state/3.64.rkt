#lang racket

(define (sqrt-stream x)
  (define guesses (stream-cons 1.0 (stream-map (lambda (guess) (/ (+ (/ x guess) guess) 2)) guesses))) guesses)

(require srfi/41)

(define answer (stream-take 10 (sqrt-stream 16)))

(define (stream-limit  s tolerance)
  (let ((first (stream-car s))
    (second (stream-car (stream-cdr s))))
    (if (< (abs (- first second)) tolerance) second (stream-limit (stream-cdr s) tolerance))
    ))

(define (sqrt x tolerance)
  (stream-limit (sqrt-stream x) tolerance))

(require rackunit)

(check-= (sqrt 256 1) 16 0.02)

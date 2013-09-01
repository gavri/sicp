#lang racket
(define (tolerance) 0.00001)
(define (average-damp current previous) (/ (+ current previous) 2))
(define (dont-damp current previous) current)
(define (fixed-point f first-guess next-guess)
  (define (close-enough? v1 v2)
    (< (abs (- v1 v2)) (tolerance)))
  (define (try guess)
    (let ((next (next-guess (f guess) guess)))
      (display next)
      (newline)
      (if (close-enough? guess next)
          next
          (try next))))
  (try first-guess))

(define (fixed-point-with-average-damping f first-guess) (fixed-point f first-guess average-damp))
(define (fixed-point-without-average-damping f first-guess) (fixed-point f first-guess dont-damp))

(define (fixed-point-function x) (/ (log 1000) (log x)))

(display "with average damping")
(newline)
(define result1 (fixed-point-with-average-damping fixed-point-function 2.0))
(display "without average damping")
(newline)
(define result2 (fixed-point-without-average-damping fixed-point-function 2.0))
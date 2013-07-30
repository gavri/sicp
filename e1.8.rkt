#lang racket
(define (cube-root-iter guess x)
  (if (good-enough? guess x)
    guess
    (cube-root-iter (improve guess x)
               x)))
(define (improve guess x)
  (/ (+ (/ x (expt guess 2)) (* 2 guess)) 3))
(define (average x y)
  (/ (+ x y) 2))
(define (good-enough? guess x)
  (< (abs (- ((curryr expt 3) guess) x)) 0.001))

(define (cube-root x)
  (cube-root-iter 1.0 x))

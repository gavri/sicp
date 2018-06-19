#lang racket

(define (monte-carlo experiment) (monte-carlo-recur 0 0 experiment))

(define (monte-carlo-recur trials-passed trials-run experiment)
  (if (experiment)
    (monte-carlo-recur-co (+ trials-passed 1) (+ trials-run 1) experiment)
    (monte-carlo-recur-co trials-passed (+ trials-run 1) experiment)))

(define (monte-carlo-recur-co trials-passed trials-run experiment)
  (stream-cons (/ trials-passed trials-run) (monte-carlo-recur trials-passed trials-run experiment)))

(define (random-float-in-range from to)
  (+ (* (random) (- to from)) from)
  )

(define (estimate-integral p x1 y1 x2 y2)
  (monte-carlo (lambda ()
                 (let ((x (random-float-in-range x1 x2))
                       (y (random-float-in-range y1 y2)))
                   (p x y)))))

(define estimate-pi
  (stream-map (lambda (x) (* x 4)) (estimate-integral (lambda (x y) (<= (+ (* x x) (* y y)) 1)) -1 -1 1 1)))

(require rackunit)
(require srfi/41)

(check-= (stream-ref estimate-pi 100000) 3.14 0.01)

#lang racket

(define (monte-carlo trials experiment)
  (define (iter trials-remaining trials-passed)
    (cond ((= trials-remaining 0)
           (/ trials-passed trials))
          ((experiment)
           (iter (- trials-remaining 1) (+ trials-passed 1)))
          (else
            (iter (- trials-remaining 1) trials-passed))))
  (iter trials 0))

(define (random-float-in-range from to)
  (+ (* (random) (- to from)) from)
  )

(define (estimate-integral p x1 y1 x2 y2 number-of-trials)
  (monte-carlo number-of-trials (lambda ()
                                  (let ((x (random-float-in-range x1 x2))
                                        (y (random-float-in-range y1 y2)))
                                    (p x y)
                                    )))
  )

(define (estimate-pi)
  (* 4 (estimate-integral (lambda (x y) (<= (+ (* x x) (* y y)) 1)) -1 -1 1 1 100000))
  )

(require rackunit)
(check-= (estimate-pi) 3.14 0.01)

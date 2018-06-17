#lang racket

(require srfi/41)

(define (add-streams s1 s2) (stream-map + s1 s2))

(define (scale-stream s x) (stream-map (lambda (e) (* e x)) s))

(define (integral delayed-integrand initial-value dt)
  (define int
    (stream-cons initial-value
                 (let ((integrand (force delayed-integrand)))
                   (add-streams (scale-stream integrand dt)
                                int))))
  int)

(define (solve-2nd a b dt y0 dy0)
  (define y (integral (delay dy) y0 dt))
  (define dy (integral (delay ddy) dy0 dt))
  (define ddy (add-streams
                (scale-stream dy a)
                (scale-stream y b)))
  y
  )

(require rackunit)

(check-= (stream-ref (solve-2nd 10 5 0.001 100 200) 1000) 760207.360347876 0.0000001)

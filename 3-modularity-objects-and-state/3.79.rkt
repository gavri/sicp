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

(define (solve-2nd f dt y0 dy0)
  (define y (integral (delay dy) y0 dt))
  (define dy (integral (delay ddy) dy0 dt))
  (define ddy (f dy y))
  y
  )

(require rackunit)

(define (f dy y)
  (add-streams
    (scale-stream dy 10)
    (scale-stream y 5)))

(check-= (stream-ref (solve-2nd f 0.001 100 200) 1000) 760207.360347876 0.0000001)

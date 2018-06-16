#lang racket

(require srfi/41)

(define (integral delayed-integrand initial-value dt)
  (stream-cons initial-value
               (let ((integrand (force delayed-integrand)))
                 (if (stream-null? integrand)
                   empty-stream
                   (integral (stream-cdr integrand)
                             (+ (* dt (stream-car integrand))
                                initial-value)
                             dt)))))

(define (solve f y0 dt)
  (define y (integral (delay dy) y0 dt))
  (define dy (stream-map f y))y)

(require rackunit)

(check-= (stream-ref (solve (lambda (y) y) 1 0.001) 1000) 2.716924 0.0000001)

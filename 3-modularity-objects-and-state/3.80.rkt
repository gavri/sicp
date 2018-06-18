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
(define (rlc r l c dt)
  (lambda (vc0 il0)
    (define vc (integral (delay dvc) vc0 dt))
    (define il (integral (delay dil) il0 dt))
    (define dvc (scale-stream il (/ -1 c)))
    (define dil (add-streams
                  (scale-stream vc (/ 1 l))
                  (scale-stream il (/ (- r) l))))
    (cons vc il)
    )
  )

(require rackunit)

(define (take-4 s) (stream->list (stream-take 4 s)))

(define result ((rlc 1 1 0.2 0.1) 10 0))
(define vc (car result))
(define il (cdr result))
(check-equal? (take-4 vc) '(10 10 9.5 8.55))
(check-equal? (take-4 il) '(0 1.0 1.9 2.66))

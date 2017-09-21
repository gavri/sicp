#lang racket

(define (create-f)
  (let ((v 'not-set))
    (lambda (x)
      (if (eq? v 'not-set)
        (begin (set! v 'set) x)
        0))))

(require rackunit)

(define f (create-f))
(check-equal? (+ (f 0) (f 1)) 0)

(define f2 (create-f))
(check-equal? (+ (f2 1) (f2 0)) 1)

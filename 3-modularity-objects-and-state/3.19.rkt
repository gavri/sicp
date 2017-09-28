#lang racket
(require compatibility/mlist)

(define (mcddr x) (mcdr (mcdr x)))

(define (detect-cycle y)
  (cond ((null? y) false)
        ((null? (mcdr y)) false)
        ((null? (mcdr (mcdr y))) false)
        (else
          (define (detect-cycle-recur slow-pointer fast-pointer)
            (cond ((null? fast-pointer) false)
                  ((null? (mcdr fast-pointer)) false)
                  ((eq? slow-pointer fast-pointer) true)
                  (else (detect-cycle-recur (mcdr slow-pointer) (mcddr fast-pointer)))))
          (detect-cycle-recur (mcdr y) (mcddr y)))))

(require rackunit)

(define (last-pair x)
  (if (null? (mcdr x))
    x
    (last-pair (mcdr x))))

(define (make-cycle x)
  (set-mcdr! (last-pair x) x)
  x)

(check-pred detect-cycle (make-cycle (mlist 1 2 3)))
(check-false (detect-cycle (mlist 1 2 3)))

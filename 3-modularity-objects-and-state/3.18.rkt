#lang racket
(require compatibility/mlist)

(define (detect-cycle y)
  ((lambda (seen)
    (define (detect-cycle-recur x)
        (cond ((null? x) false)
              ((memq (mcar x) seen) true)
              (else
               (begin
                 (set! seen (cons (mcar x) seen))
                 (detect-cycle-recur (mcdr x))))))
    (detect-cycle-recur y)) '()))

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

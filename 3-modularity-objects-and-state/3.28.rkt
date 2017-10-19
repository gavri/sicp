#lang racket

(define (get-signal wire) error "not implemented")
(define (set-signal! wire new-value) error "not implemented")
(define (after-delay duration proc) error "not implemented")
(define (add-action! wire proc) error "not implemented")
(define or-gate-delay 1)

(define (or-gate a1 a2 output)
  (define (or-action-procedure)
    (let ((new-value
            (logical-or (get-signal a1) (get-signal a2))))
      (after-delay or-gate-delay
                   (lambda ()
                     (set-signal! output new-value)))))
  (add-action! a1 or-action-procedure)
  (add-action! a2 or-action-procedure)
  'ok)

(define (logical-or s1 s2)
  (cond ((and (= s1 0) (= s2 0)) 0)
        (else 1)))

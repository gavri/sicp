#lang racket

(define (get-signal wire) error "not implemented")
(define (set-signal! wire new-value) error "not implemented")
(define (after-delay duration proc) error "not implemented")
(define (add-action! wire proc) error "not implemented")
(define inverter-delay 1)
(define and-gate-delay 2)
(define (make-wire) error "not implemented")

(define (inverter input output)
  (define (invert-input)
    (let ((new-value (logical-not (get-signal input))))
      (after-delay inverter-delay
                   (lambda ()
                     (set-signal! output new-value)))))
  (add-action! input invert-input)
  'ok)

(define (logical-not s)
  (cond ((= s 0) 1)
        ((= s 1) 0)
        (else (error "Invalid signal" s))))

(define (and-gate a1 a2 output)
  (define (and-action-procedure)
    (let ((new-value
            (logical-and (get-signal a1) (get-signal a2))))
      (after-delay and-gate-delay
                   (lambda ()
                     (set-signal! output new-value)))))
  (add-action! a1 and-action-procedure)
  (add-action! a2 and-action-procedure)
  'ok)

(define (logical-and s1 s2)
  (cond ((and (= s1 1) (= s2 1)) 1)
        (else 0)))

(define (or-gate a1 a2 output)
  (let ((not-a1 (make-wire))
        (not-a2 (make-wire))
        (and-not-a1-not-2 (make-wire)))
    (inverter a1 not-a1)
    (inverter a2 not-a2)
    (and-gate not-a1 not-a2 and-not-a1-not-2)
    (inverter and-not-a1-not-2 output)
    ))

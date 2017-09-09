#lang racket

(define (square x) (* x x))

(define (make-from-real-imag x y)
  (define (dispatch op)
    (cond ((eq? op 'real-part) x)
          ((eq? op 'imag-part) y)
          ((eq? op 'magnitude)
           (sqrt (+ (square x) (square y))))
          ((eq? op 'angle) (atan y x))
          (else
            (error "Unknown op -- MAKE-FROM-REAL-IMAG" op))))
  dispatch)

(define (make-from-mag-ang magnitude angle)
  (define (dispatch op)
    (cond ((eq? op 'real-part) (* magnitude (cos angle)))
          ((eq? op 'imag-part) (* magnitude (sin angle)))
          ((eq? op 'magnitude) magnitude)
          ((eq? op 'angle) angle)
          (else
            (error "Unknown op -- MAKE-FROM-REAL-IMAG" op))))
  dispatch)

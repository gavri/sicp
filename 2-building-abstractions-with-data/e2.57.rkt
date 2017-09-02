#lang racket

(include "symbolic-differentiation.rkt")

(require  rackunit)

(define (^ base exponent)
  (cond
    [(= exponent 0) 1]
    [(= exponent 1) base]
    [else (* base (^ base (- exponent 1)))]
    ))

(check-equal? (^ 2 3) 8)
(check-equal? (^ 2 1) 2)
(check-equal? (^ 2 0) 1)

(define (make-exponentiation base exponent)
  (cond ((=number? exponent 0) 1)
        ((=number? exponent 1) base)
        ((and (number? base) (number? exponent)) (^ base exponent))
        (else (list '^ base exponent))))

(define (exponentiation? expression)
  (and (pair? expression) (eq? (car expression) '^)))

(define (base exp) (second exp))
(define (exponent exp) (third exp))

(define (augend-multiple s)
  (if (null? (cdddr s))
    (caddr s)
    (cons '+ (cddr s))))

(define (multiplicand-multiple s)
  (if (null? (cdddr s))
    (caddr s)
    (cons '* (cddr s))))

(define (deriv-2 exp var)
  (cond ((number? exp) 0)
        ((variable? exp)
         (if (same-variable? exp var) 1 0))
        ((sum? exp)
         (make-sum (deriv-2 (addend exp) var)
                   (deriv-2 (augend-multiple exp) var)))
        ((product? exp)
         (make-sum
           (make-product (multiplier exp)
                         (deriv-2 (multiplicand-multiple exp) var))
           (make-product (deriv-2 (multiplier exp) var)
                         (multiplicand-multiple exp))))
        ((exponentiation? exp)
         (make-product
           (make-product (exponent exp) (make-exponentiation (base exp) (make-sum (exponent exp) -1)))
           (deriv-2 (base exp) var)
           ))
         (else
           (error "unknown expression type -- DERIV" exp))))


(check-equal? (deriv-2 '(+ x 3) 'x) 1)
(check-equal? (deriv-2 '(* x y) 'x) 'y)
(check-equal? (deriv-2 '(* (* x y) (+ x 3)) 'x)
              '(+ (* x y) (* y (+ x 3))))
(check-equal? (deriv-2 '(^ x 2) 'x) '(* 2 x))
(check-equal? (deriv-2 '(+ x (^ x 2)) 'x) '(+ 1 (* 2 x)))
(check-equal? (deriv-2 '(+ x (^ x y)) 'x) '(+ 1 (* y (^ x (+ y -1)))))

(check-equal? (deriv-2 '(* x y (+ x 3)) 'x)
              '(+ (* x y) (* y (+ x 3))))

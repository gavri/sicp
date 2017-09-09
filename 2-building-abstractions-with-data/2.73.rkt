#lang racket

(require racket/generic)

(define (debug x) (println x) x)

(define (variable? x) (symbol? x))

(define (same-variable? v1 v2)
  (and (variable? v1) (variable? v2) (eq? v1 v2)))

(define (=number? exp num)
  (and (number? exp) (= exp num)))

(define-generics exp
                 [deriv exp var]
                 [raw exp]
                 )

(struct sum (addend augend)
        #:methods gen:exp
        [
         (define (deriv exp var)
           (match exp
                  [
                   (sum addend augend)
                   (make-sum (deriv-full addend var) (deriv-full augend var))
                   ]
                  ))
         ]
        )

(struct product (multiplier multiplicand)
        #:methods gen:exp
        [
         (define (deriv exp var)
           (match exp
                  [
                   (product multiplier multiplicand)
                   (make-sum
                     (make-product multiplier (deriv-full multiplicand var))
                     (make-product (deriv-full multiplier var) multiplicand))
                   ]
                  ))
         ]
        )

(struct exponentiation (base exponent)
        #:methods gen:exp
        [
         (define (deriv exp var)
           (match exp
                  [
                   (exponentiation base exponent)
                   (make-product
                     (make-product exponent (make-exponentiation base (make-sum exponent -1)))
                     (deriv-full base var)
                     )
                   ]
                  ))
         ]
        )

(define (make-sum a1 a2)
  (cond ((=number? a1 0) a2)
        ((=number? a2 0) a1)
        ((and (number? a1) (number? a2)) (+ a1 a2))
        (else (sum a1 a2))))

(define (make-product m1 m2)
  (cond ((or (=number? m1 0) (=number? m2 0)) 0)
        ((=number? m1 1) m2)
        ((=number? m2 1) m1)
        ((and (number? m1) (number? m2)) (* m1 m2))
        (else (sum m1 m2))))

(define (^ base exponent)
  (cond
    [(= exponent 0) 1]
    [(= exponent 1) base]
    [else (* base (^ base (- exponent 1)))]
    ))

(define (make-exponentiation base exponent)
  (cond ((=number? exponent 0) 1)
        ((=number? exponent 1) base)
        ((and (number? base) (number? exponent)) (^ base exponent))
        (else (list '^ base exponent))))

(define (deriv-full exp var)
  (cond ((number? exp) 0)
        ((variable? exp) (if (same-variable? exp var) 1 0))
        (else (deriv exp var))))

(require rackunit)

(check-equal? (deriv-full (sum 'x 3) 'x) 1)
(check-equal? (deriv-full (product 'x 'y) 'x) 'y)

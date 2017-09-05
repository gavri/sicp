#lang racket
(require srfi/1)

(define (deriv exp var)
  (cond ((number? exp) 0)
        ((variable? exp)
         (if (same-variable? exp var) 1 0))
        ((sum? exp)
         (make-sum (deriv (addend exp) var)
                   (deriv (augend exp) var)))
        ((product? exp)
         (make-sum
           (make-product (multiplier exp)
                         (deriv (multiplicand exp) var))
           (make-product (deriv (multiplier exp) var)
                         (multiplicand exp))))
        (else
          (error "unknown expression type -- DERIV" exp))))

(define (variable? x) (symbol? x))

(define (same-variable? v1 v2)
  (and (variable? v1) (variable? v2) (eq? v1 v2)))

(define (=number? exp num)
  (and (number? exp) (= exp num)))

(define (addend s) (take-while (lambda (x) (not (eq? x '+))) s))
(define (augend s) (cdr (drop-while (lambda (x) (not (eq? x '+))) s)))

(define (multiplier p) (take-while (lambda (x) (not (eq? x '*))) p))
(define (multiplicand p) (cdr (drop-while (lambda (x) (not (eq? x '*))) p)))

(define (make-sum a1 a2)
  (cond ((=number? a1 0) a2)
        ((=number? a2 0) a1)
        ((and (number? a1) (number? a2)) (+ a1 a2))
        (else (list a1 '+ a2))))

(define (make-product m1 m2)
  (cond ((or (=number? m1 0) (=number? m2 0)) 0)
        ((=number? m1 1) m2)
        ((=number? m2 1) m1)
        ((and (number? m1) (number? m2)) (* m1 m2))
        (else (list m1 '* m2))))

(define (sum? x)
  (and (pair? x) (eq? (lowest-precedence-operator x) '+)))

(define (product? x)
  (and (pair? x) (eq? (lowest-precedence-operator x) '*)))

(define (lowest-precedence-operator exp)
  (foldr (lambda (lowest-so-far new) (if (> (precedence-level new) (precedence-level lowest-so-far)) new lowest-so-far))  null exp)
  )

(define (precedence-level op)
  (cond ((eq? op '+) 2)
        ((eq? op '*) 1)
        (else 0))
  )

(require  rackunit)

(check-equal? (deriv '(x + 3) 'x) 1)
(check-equal? (deriv '(x * y) 'x) 'y)
(check-equal? (deriv '(x * y * (x + 3)) 'x) '((x * y) + (y * (x + 3))))

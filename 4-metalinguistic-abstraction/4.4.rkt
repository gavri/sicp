#lang racket

(require rackunit)

(define (direct-expressions)
  (define (true? x) (not (eq? x 'false)))

  (define (and? exp) (and (pair? exp) (eq? (car exp) 'and)))
  (define (or? exp) (and (pair? exp) (eq? (car exp) 'or)))

  (define (eval-and exp)
    (cond ((null? exp) true)
          ((null? (cdr exp)) (car exp))
          ((eval (true? (car exp))) (eval-and (cdr exp)))
          (else false)))

  (define (eval-or exp)
    (cond ((null? exp) false)
          ((eval (true? (car exp))) (eval (car exp)))
          (else (eval-or (cdr exp)))))

  (define (eval exp)
    (cond ((and? exp) (eval-and (cdr exp)))
          ((or? exp) (eval-or (cdr exp)))
          (else exp)))

  (check-equal? (eval '(and 1 2 false 3)) false)
  (check-equal? (eval '(and 1 2 3)) 3)
  (check-equal? (eval '(and)) true)

  (check-equal? (eval '(or false 1 false 2)) 1)
  (check-equal? (eval '(or false false)) false)
  (check-equal? (eval '(or)) false)
  )

(direct-expressions)

(define (derived-expressions)
  (define (true? x) (not (eq? x 'false)))

  (define (if-predicate exp) (cadr exp))
  (define (if-consequent exp) (caddr exp))
  (define (if-alternative exp)
    (if (not (null? (cdddr exp)))
      (cadddr exp)
      'false))

  (define (make-if predicate consequent alternative)
    (list 'if predicate consequent alternative))

  (define (and? exp) (and (pair? exp) (eq? (car exp) 'and)))
  (define (or? exp) (and (pair? exp) (eq? (car exp) 'or)))
  (define (if? exp) (and (pair? exp) (eq? (car exp) 'if)))

  (define (eval-if exp)
    (if (true? (if-predicate exp))
      (eval (if-consequent exp))
      (eval (if-alternative exp))))

  (define (and->if exp)
    (cond ((null? exp) 'true)
          ((eq? (cdr exp) '()) (car exp))
      (else (make-if (car exp) (and->if (cdr exp)) (car exp))))
    )

  (define (or->if exp)
    (cond ((null? exp) 'false)
          ((eq? (cdr exp) '()) (car exp))
      (else (make-if (car exp) (car exp) (or->if (cdr exp))))))

  (define (eval exp)
    (cond ((and? exp) (eval (and->if (cdr exp))))
          ((or? exp) (eval (or->if (cdr exp))))
          ((if? exp) (eval-if exp))
          ((eq? exp 'true) true)
          ((eq? exp 'false) false)
          (else exp)))

  (check-equal? (eval '(and 1 2 false 3)) false)
  (check-equal? (eval '(and 1 2 3)) 3)
  (check-equal? (eval '(and)) true)

  (check-equal? (eval '(or false 1 false 2)) 1)
  (check-equal? (eval '(or false false)) false)
  (check-equal? (eval '(or)) false)
  )

(derived-expressions)

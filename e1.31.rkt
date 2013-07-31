#lang racket
(define (inc n) (+ n 1))
(define (bump-up-to-odd-if-even n)
  (cond
    [(even? n) (inc n)]
    [else n]
    )
  )
(define (bump-up-to-even-if-odd n)
  (cond
    [(odd? n) (inc n)]
    [else n]
    )
  )
(define (product-recursive term a next b)
  (if (> a b)
    1
    (* (term a)
       (product-recursive term (next a) next b)))
  )

(define (factorial n)
  (product-recursive identity 2 inc n)
  )

(define (term i) (/ (bump-up-to-even-if-odd i) (bump-up-to-odd-if-even i)))

(define (pi-with-product-recursive precision) 
  (* 4 (product-recursive term 2.0 inc precision))
  )

(define (product-iterative term a next b)
  (product-iter term a next b 1)
  )

(define (product-iter term a next b acc)
  (if (> a b)
    acc 
    (product-iter term (next a) next b (* acc (term a))))
  )

(define (pi-with-product-iterative precision) 
  (* 4 (product-iterative term 2.0 inc precision))
  )

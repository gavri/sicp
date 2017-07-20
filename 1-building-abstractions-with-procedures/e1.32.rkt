#lang racket
(define (accumulate-recursive term a next b f init)
  (if (> a b)
    init
    (f (term a)
       (accumulate-recursive term (next a) next b f init)))
  )
(define (accumulate-iterative term a next b f init)
  (accumulate-iter term a next b f init)
  )

(define (accumulate-iter term a next b f acc)
  (if (> a b)
    acc 
    (accumulate-iter term (next a) next b f (f acc (term a))))
  )

(define (sum-with-accumulate-recursive term a next b)
  (accumulate-recursive term a next b + 0)
  )
(define (sum-with-accumulate-iterative term a next b)
  (accumulate-iterative term a next b + 0)
  )
(define (product-with-accumulate-recursive term a next b)
  (accumulate-recursive term a next b * 1)
  )
(define (product-with-accumulate-iterative term a next b)
  (accumulate-iterative term a next b * 1)
  )


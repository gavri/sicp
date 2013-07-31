#lang racket
(define (square n) (* n n))

(define (inc n) (+ n 1))

(define (smallest-divisor n)
  (find-divisor n 2))

(define (find-divisor n test-divisor)
  (cond ((> (square test-divisor) n) n)
        ((divides? test-divisor n) test-divisor)
        (else (find-divisor n (+ test-divisor 1)))))

(define (divides? a b)
  (= (remainder b a) 0))

(define (prime? n)
  (= n (smallest-divisor n)))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (next-with-filter next predicate a)
  (let ([next-a (next a)])
    (cond 
      [(predicate next-a) next-a]
      [else (next-with-filter next predicate next-a)]
      )
    )
  )

(define (filtered-accumulate term a next b f init predicate)
  (accumulate-iter term a next b f init predicate)
  )

(define (accumulate-iter term a next b f acc predicate)
  (if (> a b)
    acc 
    (cond [(predicate a)
           (accumulate-iter term (next-with-filter next predicate a) next b f (f acc (term a)) predicate)
           ]
          [else
            (accumulate-iter term (next-with-filter next predicate a) next b f acc predicate)
            ]
          )
    )
  )
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (sum-of-squares-of-prime-numbers a b)
  (filtered-accumulate square a inc b + 0 prime?)
  )
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (gcd a b)
  (if (= b 0)
    a
    (gcd b (remainder a b))))
(define (product-of-smaller-positive-integers-relatively-prime n)
  (define (relatively-prime-to-n x)
    (= 1 (gcd n x))
    )
  (filtered-accumulate identity 1 inc n * 1 relatively-prime-to-n)
  )

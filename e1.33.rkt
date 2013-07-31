#lang racket
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

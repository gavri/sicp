#lang racket
(define (sum term a next b)
  (if (> a b)
    0
    (+ (term a)
       (sum term (next a) next b)))
  )
(define (integrate f a b n)
  (let ([h (/ (- b a) n)])
    (define (y k) (f (+ a (* k h))))
    (define (coefficient i)
      (cond 
        [(or (= i 0) (= i n)) 1]
        [(odd? i) 4]
        [else 2]
        )
      )
    (define (term k) (* (coefficient k) (f (+ a (* k h)))))
    (define (next i) (+ i 1))
    (* (/ h 3) (sum term 0 next n))
    )
  )
(define (integrate-cube a b precision)
  (integrate (lambda (x) (expt x 3)) 0 1 precision)
  )
(integrate-cube 0 1 4)
(integrate-cube 0 1 1000)

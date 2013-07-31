#lang racket
(define (fermat-test n)
  (fermat-test-with n 2)
  )

(define (fermat-test-with n a)
  (cond 
    [(= n a) #t]
    [(it a n) #f]
    [else (fermat-test-with n (+ a 1))]
    )
  )

(define (it a n)
  (not (= (modulo (expt a n) n) a))
  )

(fermat-test 561)
(fermat-test 1105)
(fermat-test 1729)
(fermat-test 2465)
(fermat-test 2821)
(fermat-test 6601)

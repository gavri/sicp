#lang racket
(define (pascal x y) 
  (cond [(= y 0) 1]
        [(= y x) 1]
        [else (+ (pascal (- x 1) (- y 1)) (pascal (- x 1) y))]
        )
  )

#lang racket
(define (f-recursive n)
  (
   cond
   [(< n 3) n]
   [else
    (+
      (f-recursive (- n 1))
      (* 2 (f-recursive (- n 2)))
      (* 3 (f-recursive (- n 3)))
      )]))

(define (f-iterative n)
  (cond [(< n 3) n]
        [else (f-iter 2 1 0 n)]
        )
  )

(define (f-iter a b c count)
  (cond [(< count 3) a]
        [else (f-iter (+ a (* 2 b) (* 3 c)) a b (- count 1))]
        )
  )

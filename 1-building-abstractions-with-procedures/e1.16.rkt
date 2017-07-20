#lang racket
(define (square n) (* n n))
(define (even? n) (= (remainder n 2) 0))
(define (fast-expt-r b n)
  (cond [(= n 0) 1]
        [(even? n) (square (fast-expt-r b (/ n 2)))]
        [else (* b (fast-expt-r b (- n 1)))]))
(define (fast-expt-i b n)
  (fast-expt-i-iter b n 1)
  )

(define (fast-expt-i-iter b n a)
  (cond [(= n 0) a]
        [(even? n) (fast-expt-i-iter (square b) (/ n 2) a)]
        [else (fast-expt-i-iter b (- n 1) (* b a))]
        )
  )

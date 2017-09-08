#lang racket
(define (double n) (* n 2))
(define (halve n) (/ n 2))
(define (dec n) (- n 1))
(define (multiply a b)
  (cond
    [(= b 0) 0]
    [(even? b) (double (multiply a (halve b)))]
                       [else (+ a (multiply a (dec b)))]
        )
  )

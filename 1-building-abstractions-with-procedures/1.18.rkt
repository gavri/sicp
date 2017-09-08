#lang racket
(define (double n) (* n 2))
(define (halve n) (/ n 2))
(define (multiply a b)
  (multiply-iter a b 0)
  )

(define (multiply-iter a b acc)
     (cond
       [(= b 0) acc]
       [(even? b) (multiply-iter (double a) (halve b) acc)]
       [else (multiply-iter a (- b 1) (+ a acc))]
       )
     )

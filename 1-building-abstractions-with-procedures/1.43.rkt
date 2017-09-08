#lang racket
(define (comp f g)
	(lambda (x)
		(f (g x))))
(define (repeated f n)
	(cond
	[(= n 1) f]
	[(comp f (repeated f (- n 1)))]
)
)

#lang racket
(define (average-of-3 a b c) (/ (+ a b c) 3))
(define (comp f g)
	(lambda (x)
		(f (g x))))
(define (repeated f n)
	(cond
	[(= n 1) f]
	[(comp f (repeated f (- n 1)))]
)
)
(define dx 0.001)
(define (smooth f)
	(lambda (x)
		(average-of-3 (f (- x dx)) (f x) (f (+ x dx)))
		)
		)
(define (smooth-multiple f n)
	((repeated smooth n) f)
)
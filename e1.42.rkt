#lang racket
(define (comp f g)
	(lambda (x)
		(f (g x))))

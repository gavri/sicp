#lang racket
(define (make-interval a b) (cons a b))
(define lower-bound car)
(define upper-bound cdr)
(define (sub-interval a b) (make-interval 
                                 (- (lower-bound a) (upper-bound b))
				 (- (upper-bound a) (lower-bound b))
				 )
)
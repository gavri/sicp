#lang racket
(define (reverse-r sequence)
  (foldr (lambda (x y) (append y (list x))) null sequence))
(define (reverse-l sequence)
  (foldl cons null sequence))

(require rackunit)
(check-equal? (reverse-r '(1 2 3 4)) '(4 3 2 1))
(check-equal? (reverse-l '(1 2 3 4)) '(4 3 2 1))

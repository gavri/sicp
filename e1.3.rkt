#lang racket
(require srfi/1)
(define (two-biggest xs) (take (sort xs >) 2))
(define (sum xs) (reduce + 0 xs))
(define (add-squares xs) (sum (map (curryr expt 2) xs)))
(define (add-squares-of-largest a b c) (add-squares  (two-biggest (list a b c))))

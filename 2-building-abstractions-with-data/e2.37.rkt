#lang racket
(require srfi/1)
(require "e2.36.rkt")

(define (foldr-n op init seqs)
  (if (null? (car seqs)) null
    (cons (foldr op init (map car seqs))
          (foldr-n op init (map cdr seqs)))))

(define (dot-product v w)
  (foldr + 0 (map * v w)))

(define (matrix-*-vector m v)
  (map (lambda (u) (dot-product u v)) m))

(define (transpose mat)
  (foldr-n cons '() mat))

(define (matrix-*-matrix m n)
  (let ((cols (transpose n)))
    (map (lambda (m-row) (matrix-*-vector cols m-row)) m)))

(require rackunit)
(check-equal? (matrix-*-vector '((1 2 3) (4 5 6) (7 8 9)) '(10 20 30)) '(140 320 500))
(check-equal? (transpose '((1 2 3) (4 5 6) (7 8 9))) '((1 4 7) (2 5 8) (3 6 9)))
(check-equal? (matrix-*-matrix '((1 2 3) (4 5 6) (7 8 9)) '((2 4 6) (10 20 30) (100 200 300))) '((322 644 966) (658 1316 1974) (994 1988 2982)))

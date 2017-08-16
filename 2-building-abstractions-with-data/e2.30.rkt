#lang racket
(require rackunit)
(define (square x) (* x x))
(define (square-tree-by-recursion items)
  (cond
    [(null? items) '()]
    [else (let ((head (car items)))
            (cond
              [(list? (car items)) (cons (square-tree-by-recursion (car items)) (square-tree-by-recursion (cdr items))) ]
              [else (cons (square (car items)) (square-tree-by-recursion (cdr items)))]
              ))]))
(define (square-tree-by-map element)
  (cond
    [(list? element) (map square-tree-by-map element)]
    [else (square element)]
    ))

(check-equal? (square-tree-by-recursion '(1 2 3)) '(1 4 9))
(check-equal? (square-tree-by-recursion '(1 2 (3 4))) '(1 4 (9 16)))
(check-equal? (square-tree-by-recursion '(1 2 (3 (4 5)) 6)) '(1 4 (9 (16 25)) 36))
(check-equal? (square-tree-by-recursion '()) '())

(check-equal? (square-tree-by-map '(1 2 3)) '(1 4 9))
(check-equal? (square-tree-by-map '(1 2 (3 4))) '(1 4 (9 16)))
(check-equal? (square-tree-by-map '(1 2 (3 (4 5)) 6)) '(1 4 (9 (16 25)) 36))
(check-equal? (square-tree-by-map '()) '())

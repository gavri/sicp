#lang racket
(require rackunit)

(define (tree-map f tree)
  (cond
    [(null? tree) '()]
    [else (let ((head (car tree)))
            (cond
              [(list? (car tree)) (cons (tree-map f (car tree)) (tree-map f (cdr tree))) ]
              [else (cons (f (car tree)) (tree-map f (cdr tree)))]
              ))]))

(define (square x) (* x x))
(define (square-tree tree) (tree-map square tree))

(check-equal? (square-tree '(1 2 3)) '(1 4 9))
(check-equal? (square-tree '(1 2 (3 4))) '(1 4 (9 16)))
(check-equal? (square-tree '(1 2 (3 (4 5)) 6)) '(1 4 (9 (16 25)) 36))
(check-equal? (square-tree '()) '())

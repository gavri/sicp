#lang racket
(require rackunit)
(define (fringe input)
  (cond
    [(null? input) '()]
    [(pair? input) (apply append (map fringe input))]
    [else (list input)]
    )
  )
(check-equal? (fringe '(1 2 3 4)) '(1 2 3 4))
(check-equal? (fringe '((1 2 3 4))) '(1 2 3 4))
(check-equal? (fringe '((1 2) (3 4))) '(1 2 3 4))
(check-equal? (fringe '((1) (2 3) 4)) '(1 2 3 4))

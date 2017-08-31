#lang racket

(define (my-equal? a b)
  (cond [(and (null? a) (null? b)) true]
        [(or (null? a) (null? b)) false]
        [else (and (eq? (car a) (car b)) (my-equal? (cdr a) (cdr b)))]
        )
  )

(require rackunit)
(check-equal? (my-equal? '(a b c) '(a b c)) true)
(check-equal? (my-equal? '(a) '(a)) true)
(check-equal? (my-equal? '(a b c) '(a b)) false)
(check-equal? (my-equal? '(c) '(a b)) false)
(check-equal? (my-equal? '() '()) true)

#lang racket
(require rackunit)
(define (deep-reverse l)
  (define (deep-reverse-iter l a)
    (cond
      [(null? l) a]
      [else (deep-reverse-iter (cdr l) (cons
                                         (let [(head (car l))]
                                           (if (pair? head) (deep-reverse head) head)
                                           )
                                         a))]
      )
    )
  (deep-reverse-iter l '())
  )
(check-equal? (deep-reverse '(9 7 8)) '(8 7 9) "Multiple")
(check-equal? (deep-reverse '(8)) '(8) "Single")
(check-equal? (deep-reverse '()) '() "Empty")
(check-equal? (deep-reverse '((1 2) (3 4))) '((4 3) (2 1)) "Deep")
(check-equal? (deep-reverse '((1 2) ())) '(() (2 1)) "Deep and Empty")

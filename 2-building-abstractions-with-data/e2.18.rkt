(require rackunit)
(define (reverse l)
  (define (reverse-iter l a)
     (cond
       [(null? l) a]
       [else (reverse-iter (cdr l) (cons (car l) a))]
       )
     )
  (reverse-iter l '())
)
(check-equal? (reverse '(9 7 8)) '(8 7 9) "Multiple")
(check-equal? (reverse '(8)) '(8) "Single")
(check-equal? (reverse '()) '() "Empty")

(require rackunit)
(define (last-pair l)
     (cond
       [(null? (cdr l)) l]
       [else (last-pair (cdr l))]
       )
  )
(check-equal? (last-pair '(1 2 3 4)) '(4) "Multiple element list")
(check-equal? (last-pair '(1)) '(1) "Single element list")

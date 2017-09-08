#lang racket
(require srfi/1)
(define (fold-n op init seqs)
  (if (null? (car seqs)) null
    (cons (fold op init (map car seqs))
          (fold-n op init (map cdr seqs)))))

(require rackunit)
(check-equal? (fold-n + 0 '((1 2 3) (4 5 6) (7 8 9) (10 11 12))) '(22 26 30))


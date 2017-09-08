#lang racket
(require srfi/1)
(define (my-map p sequence)
  (fold (lambda (x acc) (cons (p x) acc)) '() (reverse sequence)))
(define (my-append seq1 seq2)
  (fold cons seq2 (reverse seq1)))
(define (my-length sequence)
  (fold (lambda (_x acc) (+ acc 1)) 0 sequence))

(require rackunit)
(check-equal? (my-map (lambda (x) (* x 2)) '(1 2 3)) '(2 4 6))
(check-equal? (my-append '(1 2 3) '(4 5 6)) '(1 2 3 4 5 6))
(check-equal? (my-length '(1 2 3 1 2 3)) 6)

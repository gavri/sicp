#lang racket

(define (accumulate op initial sequence)
  (if (null? sequence)
    initial
    (op (car sequence)
        (accumulate op initial (cdr sequence)))))

(define (flatmap proc seq)
  (accumulate append null (map proc seq)))

(define (enumerate-interval low high)
  (if (> low high)
    null
    (cons low (enumerate-interval (+ low 1) high))))

(define (unique-triples n)
  (flatmap
    (lambda (i)
      (flatmap (lambda (j) (map (lambda (k) (list k j i)) (enumerate-interval 1 (- j 1))))
               (enumerate-interval 1 (- i 1))))
    (enumerate-interval 1 n)))

(require rackunit)
(check-equal? (unique-triples 4) '((1 2 3) (1 2 4) (1 3 4) (2 3 4)))

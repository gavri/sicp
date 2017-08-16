#lang racket
(require rackunit)
(define (make-prepender head)
  (lambda (list) (cons head list)))

(define (subsets s)
  (if (null? s)
    (list '())
    (let ((rest (subsets (cdr s))))
      (append rest (map (make-prepender (car s)) rest)))))

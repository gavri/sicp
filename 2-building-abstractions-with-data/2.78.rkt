#lang racket

(define (attach-tag type-tag contents)
  (if (eq? type-tag 'scheme-number)
    contents
  (cons type-tag contents)
  ))
(define (type-tag datum)
  (if (pair? datum)
    (car datum)
    'scheme-number))
(define (contents datum)
  (if (pair? datum)
    (cdr datum)
    datum))

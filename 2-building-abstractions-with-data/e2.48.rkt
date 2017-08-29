#lang racket

(define (make-segment x y)
  (cons x y))

(define start-segment car)
(define end-segment cdr)

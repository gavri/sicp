#lang racket
(define (for-each procedure items)
  (when (not (null? items))
    (begin
      (procedure (car items))
      (for-each procedure (cdr items))
      )
    ))

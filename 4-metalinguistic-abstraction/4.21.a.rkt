#lang racket

(define fibonacci (lambda (n)
  ((lambda (fibonacci-as-arg)
     (fibonacci-as-arg fibonacci-as-arg n))
   (lambda (fb k)
     (cond ((= k 0) 0)
           ((= k 1) 1)
           (else (+ (fb fb (- k 2)) (fb fb (- k 1)))))))))

(require rackunit)

(check-equal? (fibonacci 0) 0)
(check-equal? (fibonacci 1) 1)
(check-equal? (fibonacci 2) 1)
(check-equal? (fibonacci 10) 55)

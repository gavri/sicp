#lang racket

(require rackunit)

(define (make-monitored f)
  (let ((counter 0))
    (lambda (arg)
      (cond ((eq? arg 'how-many-calls?) counter)
            ((eq? arg 'reset-count) (set! counter 0))
            (else (begin (set! counter (+ counter 1)) (f arg)))))))

(define (doubler x) (* x 2))

(define subject (make-monitored doubler))

(check-equal? (subject 'how-many-calls?) 0)
(check-equal? (subject 10) 20)
(check-equal? (subject 'how-many-calls?) 1)
(check-equal? (subject 1) 2)
(check-equal? (subject 'how-many-calls?) 2)
(subject 'reset-count)
(check-equal? (subject 'how-many-calls?) 0)
(check-equal? (subject 2) 4)
(check-equal? (subject 'how-many-calls?) 1)

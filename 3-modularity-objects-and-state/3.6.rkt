#lang racket

(define (make-random-generator seed)
  (lambda (command)
    (cond ((eq? command 'generate)
           (begin
             (set! seed (modulo (* 7 seed) 1000))
             seed))
           ((eq? command 'reset)
           (lambda (new-seed)
             (set! seed new-seed)))
          )))

(require rackunit)

(define generator (make-random-generator 10))

(check-equal? (generator 'generate) 70)
(check-equal? (generator 'generate) 490)

((generator 'reset) 10)

(check-equal? (generator 'generate) 70)
(check-equal? (generator 'generate) 490)

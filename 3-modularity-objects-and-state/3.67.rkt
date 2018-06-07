#lang racket

(define (interleave s1 s2)(if (stream-null? s1)
                            s2
                            (stream-cons (stream-car s1)
                                         (interleave s2 (stream-cdr s1)))))

(define integers (stream-cons 1 (stream-map (lambda (n) (+ n 1)) integers)))

(define (pairs s t)
  (stream-cons
    (list (stream-car s) (stream-car t))
    (interleave

      (interleave
      (stream-map (lambda (x) (list (stream-car s) x))
                  (stream-cdr t))
      (pairs (stream-cdr s) (stream-cdr t)))

     (stream-map (lambda (x) (list x (stream-car t))) (stream-cdr s)))))

(require rackunit)
(require srfi/41)

(define (take-to-list n s) (stream->list (stream-take n s)))

(check-equal? (take-to-list 5 integers) '(1 2 3 4 5))
(check-equal? (stream->list (interleave (list->stream '(1 2 3)) (list->stream '(4 5 6)))) '(1 4 2 5 3 6))
(check-equal? (take-to-list 10 (pairs integers integers)) '((1 1) (1 2) (2 1) (2 2) (3 1) (1 3) (4 1) (2 3) (5 1) (1 4)))

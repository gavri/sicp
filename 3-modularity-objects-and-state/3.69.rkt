#lang racket

(define (interleave s1 s2)(if (stream-null? s1)
                            s2
                            (stream-cons (stream-car s1)
                                         (interleave s2 (stream-cdr s1)))))

(define (pairs s t)
  (stream-cons
    (list (stream-car s) (stream-car t))
    (interleave
      (stream-map (lambda (x) (list (stream-car s) x))
                  (stream-cdr t))
      (pairs (stream-cdr s) (stream-cdr t)))))

(define (triples s t u)
  (stream-cons
    (list (stream-car s) (stream-car t) (stream-car u))
  (interleave
    (stream-map (lambda (x) (cons (stream-car s) x)) (pairs (stream-cdr t) (stream-cdr u)))
    (triples (stream-cdr s) (stream-cdr t) (stream-cdr u)))))

(define (pythogorean-filter s)
  (stream-filter (lambda (x) (= (+ (expt (first x) 2) (expt (second x) 2)) (expt (third x) 2))) s)
  )

(define integers (stream-cons 1 (stream-map (lambda (n) (+ n 1)) integers)))

(define pythogorean-triples (pythogorean-filter (triples integers integers integers)))

(require rackunit)
(require srfi/41)

(define (take-to-list n s) (stream->list (stream-take n s)))

(check-equal? (take-to-list 10 (pairs integers integers)) '((1 1) (1 2) (2 2) (1 3) (2 3) (1 4) (3 3) (1 5) (2 4) (1 6)))
(check-equal? (take-to-list 10 (triples integers integers integers)) '((1 1 1) (1 2 2) (2 2 2) (1 2 3) (2 3 3) (1 3 3) (3 3 3) (1 2 4) (2 3 4) (1 3 4)))
(check-equal? (take-to-list 5 pythogorean-triples) '((3 4 5) (6 8 10) (5 12 13) (9 12 15) (8 15 17)))

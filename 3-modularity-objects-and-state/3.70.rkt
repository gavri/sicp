#lang racket

(define (not-divisible-by x n) (not (= (remainder n x) 0)))

(define (merge-weighted s1 s2 weight)
  (cond ((stream-null? s1) s2)
        ((stream-null? s2) s1)
        (else
          (let ((s1car (stream-car s1))(s2car (stream-car s2)))
            (cond ((< (weight s1car) (weight s2car))
                   (stream-cons s1car (merge-weighted (stream-cdr s1) s2 weight)))
                  (else
                   (stream-cons s2car (merge-weighted s1 (stream-cdr s2) weight))))))))

(define (weighted-pairs s t weight)
  (stream-cons
    (list (stream-car s) (stream-car t))
    (merge-weighted
      (stream-map (lambda (x) (list (stream-car s) x))
                  (stream-cdr t))
      (weighted-pairs (stream-cdr s) (stream-cdr t) weight) weight)))

(define integers (stream-cons 1 (stream-map (lambda (n) (+ n 1)) integers)))
(define a-stream integers)
(define b-stream (stream-filter (lambda (x) (and (not-divisible-by 2 x) (not-divisible-by 3 x) (not-divisible-by 5 x))) integers))
(define a (weighted-pairs a-stream a-stream (lambda (n) (+ (first n) (second n)))))
(define b (weighted-pairs b-stream b-stream (lambda (n) (+ (* (first n) 2) (* (second n) 3) (* (first n) (second n) 5)))))

(require rackunit)
(require srfi/41)

(define (take-to-list n s) (stream->list (stream-take n s)))

(check-equal? (take-to-list 5 integers) '(1 2 3 4 5))
(check-equal? (stream->list (merge-weighted (list->stream '(1 2 3)) (list->stream '(0.8 0.9 2 4)) (lambda (x) x))) '(0.8 0.9 1 2 2 3 4))
(check-equal? (take-to-list 5 a) '((1 1) (1 2) (2 2) (1 3) (2 3)))
(check-equal? (take-to-list 20 b) '((1 1) (1 7) (1 11) (1 13) (1 17) (1 19) (1 23) (1 29) (1 31) (7 7) (1 37) (1 41) (1 43) (1 47) (1 49) (1 53) (7 11) (1 59) (1 61) (7 13)))

#lang racket

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

(define (sum-of-cubes pair) (+ (expt (first pair) 3) (expt (second pair) 3)))

(define pairs-by-weight (weighted-pairs integers integers sum-of-cubes))

(define weights-in-order (stream-map sum-of-cubes pairs-by-weight))

(define (each-cons s) (each-cons-recur s (stream-cdr s)))

(define (each-cons-recur s1 s2)
  (stream-cons (list (stream-car s1) (stream-car s2)) (each-cons-recur (stream-cdr s1) (stream-cdr s2)))
  )

(define ramanujan-numbers
  (stream-map first
  (stream-filter
    (lambda (x) (= (first x) (second x)))
    (each-cons weights-in-order))))

(require rackunit)
(require srfi/41)

(define (take-to-list n s) (stream->list (stream-take n s)))

(check-equal? (take-to-list 10 weights-in-order) '(2 9 16 28 35 54 65 72 91 126))
(check-equal? (take-to-list 5 ramanujan-numbers) '(1729 4104 13832 20683 32832))

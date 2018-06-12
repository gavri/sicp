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

(define (sum-of-squares pair) (+ (expt (first pair) 2) (expt (second pair) 2)))

(define pairs-by-weight (weighted-pairs integers integers sum-of-squares))

(define weights-in-order (stream-map sum-of-squares pairs-by-weight))

(define (triple-window s) (triple-window-recur s (stream-cdr s) (stream-cdr (stream-cdr s))))

(define (triple-window-recur s1 s2 s3)
  (stream-cons (list (stream-car s1) (stream-car s2) (stream-car s3)) (triple-window-recur (stream-cdr s1) (stream-cdr s2) (stream-cdr s3)))
  )

(define ramanujan-numbers
  (stream-map first
  (stream-filter
    (lambda (x) (= (first x) (second x)))
    (triple-window weights-in-order))))

(require rackunit)
(require srfi/41)

(define (take-to-list n s) (stream->list (stream-take n s)))

(check-equal? (take-to-list 10 weights-in-order) '(2 5 8 10 13 17 18 20 25 26))
(check-equal? (take-to-list 5 ramanujan-numbers) '(50 65 85 125 130))

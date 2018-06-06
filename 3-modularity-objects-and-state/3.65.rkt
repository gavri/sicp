#lang racket

(define (stream-map-multi proc argstreams)
  (if (stream-empty? (car argstreams))
    empty-stream
    (stream-cons
      (apply proc (map stream-first argstreams))
      (apply stream-map-multi
             (list proc (map stream-rest argstreams))))))

(define (euler-transform s)
  (let ((s0 (stream-ref s 0))
        (s1 (stream-ref s 1))
        (s2 (stream-ref s 2)))
    (stream-cons (- s2 (/ (* (- s2 s1) (- s2 s1))
                          (+ s0 (* -2 s1) s2)))
                 (euler-transform (stream-cdr s)))))

(define (make-tableau transform s)
  (stream-cons s
               (make-tableau transform
                             (transform s))))

(define (accelerated-sequence transform s)
  (stream-map stream-car(make-tableau transform s)))

(define (partial-sums-recur s acc)
  (let ((curr (+ (stream-first s) acc)))
  (stream-cons curr (partial-sums-recur (stream-rest s) curr))))

(define (partial-sums s)
  (partial-sums-recur s 0))

(define natural-numbers
  (stream-cons 1
               (stream-map
                 (lambda (n) (+ n 1)) natural-numbers)))

(define reciprocals-of-natural-numbers (stream-map (lambda (n) (/ 1 n)) natural-numbers))

(define alternating-signs (stream-cons 1 (stream-map - alternating-signs)))

(define terms (stream-map-multi * (list alternating-signs reciprocals-of-natural-numbers)))

(define answer-first (partial-sums terms))
(define answer-second (euler-transform answer-first))
(define answer-third (accelerated-sequence euler-transform answer-first))

(require rackunit)
(require srfi/41)

(define (take-5 s) (stream->list (stream-take 5 s)))

(check-equal? (take-5 natural-numbers) '(1 2 3 4 5))
(check-equal? (take-5 reciprocals-of-natural-numbers) '(1 1/2 1/3 1/4 1/5))
(check-equal? (take-5 alternating-signs) '(1 -1 1 -1 1))
(check-equal? (take-5 terms) '(1 -1/2 1/3 -1/4 1/5))
(check-equal? (take-5 answer-first) '(1 1/2 5/6 7/12 47/60))
(check-equal? (take-5 answer-second) '(7/10 29/42 25/36 457/660 541/780))
(check-equal? (take-5 answer-third) '(1 7/10 165/238 380522285/548976276 755849325680052062216639661/1090460049411856348776491380))

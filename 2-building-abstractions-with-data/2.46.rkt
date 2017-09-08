#lang racket

(define (make-vect x y)
  (cons x y))

(define (xcor-vect v)
  (car v))

(define (ycor-vect v)
  (cdr v))

(define (add-vect v1 v2)
  (cons (+ (xcor-vect v1) (xcor-vect v2)) (+ (ycor-vect v1) (ycor-vect v2))))

(define (sub-vect v1 v2)
  (cons (- (xcor-vect v1) (xcor-vect v2)) (- (ycor-vect v1) (ycor-vect v2))))

(define (scale-vect s v)
  (cons (* s (xcor-vect v)) (* s (ycor-vect v))))

(require rackunit)

(check-equal? 5 (xcor-vect (make-vect 5 7)))
(check-equal? 7 (ycor-vect (make-vect 5 7)))
(check-equal? (make-vect 3 5) (add-vect (make-vect 1 2) (make-vect 2 3)))
(check-equal? (make-vect 3 5) (sub-vect (make-vect 5 8) (make-vect 2 3)))
(check-equal? (make-vect 6 10) (scale-vect 2 (make-vect 3 5)))

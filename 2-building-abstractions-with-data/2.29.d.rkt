#lang racket
(require rackunit)

(define (make-mobile left right)
  (cons left right))

(define (make-branch len structure)
  (cons len structure))

(define (left-branch mobile)
  (car mobile))

(define (right-branch mobile)
  (cdr mobile))

(define (branch-length branch)
  (car branch))

(define (branch-structure branch)
  (cdr branch))

(define (total-weight mobile)
  (+ (total-weight-branch (left-branch mobile)) (total-weight-branch (right-branch mobile))))

(define (total-weight-branch branch)
  (let ((structure (branch-structure branch)))
    (cond
      [(pair? structure) (total-weight structure)]
      [else structure]
      )
    ))

(define (balancing-info is-balanced weight torque)
  (list is-balanced weight torque))

(define (balancing-info-is-balanced balancing-info)
  (first balancing-info))

(define (balancing-info-weight balancing-info)
  (second balancing-info))

(define (balancing-info-torque balancing-info)
  (third balancing-info))

(define (balancing-info-mobile-recur mobile)
  (let ((left-balancing-info (balancing-info-branch-recur (left-branch mobile))) (right-balancing-info (balancing-info-branch-recur (right-branch mobile))))
    (balancing-info
      (and
        (balancing-info-is-balanced left-balancing-info)
        (balancing-info-is-balanced right-balancing-info)
        (= (balancing-info-torque left-balancing-info) (balancing-info-torque right-balancing-info)))
      (+ (balancing-info-weight left-balancing-info) (balancing-info-weight right-balancing-info))
      null)))

(define (balancing-info-branch-recur branch)
  (let ((len (branch-length branch)) (structure (branch-structure branch)))
    (cond
      [(pair? structure)
       (let ((struct-balancing-info (balancing-info-mobile-recur structure)))
       (balancing-info (balancing-info-is-balanced struct-balancing-info) (balancing-info-weight struct-balancing-info) (* len (balancing-info-weight struct-balancing-info)))
       )
       ]
      [else (balancing-info true structure (* len structure))]
      )))



(define (is-balanced mobile)
  (balancing-info-is-balanced (balancing-info-mobile-recur mobile)))


(check-equal? (total-weight (make-mobile (make-branch 1 3) (make-branch 1 5))) 8)
(check-equal? (total-weight (make-mobile (make-branch 1 3) (make-branch 1 (make-mobile (make-branch 1 5) (make-mobile 1 5))))) 13)
(check-equal? (is-balanced (make-mobile (make-branch 2 3) (make-branch 2 3))) true)
(check-equal? (is-balanced (make-mobile (make-branch 2 3) (make-branch 2 4))) false)
(check-equal? (is-balanced (make-mobile (make-branch 2 3) (make-branch 3 2))) true)
(check-equal? (is-balanced (make-mobile (make-branch 4 6) (make-branch 3 (make-mobile (make-branch 3 5) (make-mobile 5 3))))) true)
(check-equal? (is-balanced (make-mobile (make-branch 5 6) (make-branch 3 (make-mobile (make-branch 3 5) (make-mobile 5 3))))) false)

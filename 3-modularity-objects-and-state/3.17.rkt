#lang racket
(require compatibility/mlist)

(define (count-pairs y)
  (let ((visited '()))
    (define (count-pairs-recur x)
      (cond ((not (mpair? x)) 0)
            ((mmemq x visited) 0)
            (else (begin
                    (set! visited (mcons x visited))
                    (+ (count-pairs-recur (mcar x))
                       (count-pairs-recur (mcdr x))
                       1)))))
    (count-pairs-recur y)
    )
  )

(require rackunit)

(check-equal? (count-pairs (mlist 'a 'b)) 2)

(define a (mlist 'x 'y 'z))
(define b (mlist 'k a a a a))
(check-equal? (count-pairs b) 8)

(define x (mlist 1 2 3))
(define y (mlist 4 5 6))

(set-mcar! x y)
(set-mcdr! y x)

(check-equal? (count-pairs x) 4)

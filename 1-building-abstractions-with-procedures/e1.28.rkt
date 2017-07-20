#lang racket
(define (!= x y) (not (= x y)))
(define (square-with-check x m) 
  (cond 
    [(and (= (remainder (* x x) m) 1) (!= x 1) (!= x (- m 1))) 0]
    [else (* x x)]
  )
  )
(define (expmod base exp m)
  (cond ((= exp 0) 1)
        ((even? exp)
         (remainder (square-with-check (expmod base (/ exp 2) m) m)
                    m))
        (else
          (remainder (* base (expmod base (- exp 1) m))
                     m))))
(define (fermat-test n)
  (define (try-it a)
    (= (expmod a n n) a))
  (try-it (+ 1 (random (- n 1)))))
(define (fast-prime? n times)
  (cond ((= times 0) true)
        ((fermat-test n) (fast-prime? n (- times 1)))
        (else false)))

(fast-prime? 2 100)
(fast-prime? 3 100)
(fast-prime? 7 100)

(display "=========\n")

(fast-prime? 4 100)
(fast-prime? 8 100)
(fast-prime? 9 100)

(display "==carmichael numbers=======\n")
(fast-prime? 561 100)
(fast-prime? 1105 100)
(fast-prime? 1729 100)
(fast-prime? 2465 100)
(fast-prime? 2821 100)
(fast-prime? 6601 100)
(display "=========\n")

(fast-prime? 1009 100)
(fast-prime? 1013 100)
(fast-prime? 1019 100)

(fast-prime? 10007 100)
(fast-prime? 10009 100)
(fast-prime? 10037 100)

(fast-prime? 100003 100)
(fast-prime? 100019 100)
(fast-prime? 100043 100)

(fast-prime? 1000003 100)
(fast-prime? 1000033 100)
(fast-prime? 1000037 100)

#lang racket

(define (stream-scale s x) (stream-map (lambda (e) (* e x)) s))

(define (merge s1 s2)
  (cond ((stream-null? s1) s2)
        ((stream-null? s2) s1)
        (else
          (let ((s1car (stream-car s1))(s2car (stream-car s2)))
            (cond ((< s1car s2car)
                   (stream-cons s1car (merge (stream-cdr s1) s2)))
                  ((> s1car s2car)
                   (stream-cons s2car (merge s1 (stream-cdr s2))))
                  (else
                    (stream-cons s1car
                                 (merge (stream-cdr s1)
                                        (stream-cdr s2)))))))))


(define S (stream-cons 1 (merge (stream-scale S 2) (merge (stream-scale S 3) (stream-scale S 5)))))

(require rackunit)
(require srfi/41)

(check-equal? (stream->list (stream-take 10 S)) '(1 2 3 4 5 6 8 9 10 12))

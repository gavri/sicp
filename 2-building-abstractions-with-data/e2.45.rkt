#lang sicp
(#%require sicp-pict)

(define (split primary-splitter secondary-splitter)
  (lambda (painter n)
    (if (= n 0)
      painter
      (let ((smaller ((split primary-splitter secondary-splitter) painter (- n 1))))
        (primary-splitter painter (secondary-splitter smaller smaller))))))



(define right-split (split beside below))
(define up-split (split below beside))

(paint (up-split einstein 2))

(paint (right-split einstein 2))

#lang sicp
(#%require sicp-pict)

(define wave einstein)

(define wave-with-smile "no")

(define (split primary-splitter secondary-splitter)
  (lambda (painter n)
    (if (= n 0)
        painter
        (let ((smaller ((split primary-splitter secondary-splitter) painter (- n 1))))
          (primary-splitter painter (secondary-splitter smaller smaller))))))


(define right-split (split beside below))
(define up-split (split below beside))

(define (corner-split-with-one-copy-of-up-and-right painter n)
  (if (= n 0)
      painter
      (let ((up (up-split painter (- n 1)))
            (right (right-split painter (- n 1))))
        (let ((top-left up)
              (bottom-right right)
              (corner (corner-split-with-one-copy-of-up-and-right painter (- n 1))))
          (beside (below painter top-left)
                  (below bottom-right corner))))))


(define (square-of-four tl tr bl br)
  (lambda (painter)
    (let ((top (beside (tl painter) (tr painter)))
          (bottom (beside (bl painter) (br painter))))
      (below bottom top))))

(define (square-limit-modified painter n)
  (let ((quarter (corner-split-with-one-copy-of-up-and-right painter n)))
    (let ((half (beside quarter (flip-horiz quarter))))
      (below half (flip-vert half)))))


(paint (corner-split-with-one-copy-of-up-and-right wave 8))
(paint (square-limit-modified wave 2))

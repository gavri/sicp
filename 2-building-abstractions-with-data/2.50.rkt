#lang sicp
(#%require sicp-pict)


(define (flip-horiz painter)
  ((transform-painter
    (make-vect 1.0 0.0)
    (make-vect 0.0 0.0)
    (make-vect 1.0 1.0)) painter))

(define (counter-clockwise-180 painter)
  ((transform-painter
    (make-vect 1.0 1.0)
    (make-vect 0.0 1.0)
    (make-vect 1.0 0.0)) painter))

(define (counter-clockwise-270 painter)
  ((transform-painter
    (make-vect 1.0 0.0)
    (make-vect 1.0 1.0)
    (make-vect 0.0 0.0)) painter))



(paint (flip-horiz einstein))
(paint (counter-clockwise-180 einstein))
(paint (counter-clockwise-270 einstein))

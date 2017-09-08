#lang sicp
(#%require sicp-pict)

(define (counter-clockwise-90 painter)
  ((transform-painter
     (make-vect 0.0 1.0)
     (make-vect 0.0 0.0)
     (make-vect 1.0 1.0)) painter))

(define (clockwise-90 painter)
  ((transform-painter
     (make-vect 1.0 0.0)
     (make-vect 1.0 1.0)
     (make-vect 0.0 0.0)) painter))

(define (below-1 painter1 painter2)
  (let ((split-point (make-vect 0.0 0.5)))
    (let ((paint-bottom
            ((transform-painter
               (make-vect 0.0 0.0)
               (make-vect 1.0 0.0)
               split-point) painter1))
          (paint-top
            ((transform-painter
               split-point
               (make-vect 1.0  0.5)
               (make-vect 0.0 1.0)) painter2)))
      (lambda (frame)
        (paint-bottom frame)
        (paint-top frame)))))

(define (below-2 painter1 painter2)
  (clockwise-90 (beside (counter-clockwise-90 painter1) (counter-clockwise-90 painter2))))

(define (another-below-2 painter1 painter2)
  (counter-clockwise-90 (beside (clockwise-90 painter1) (clockwise-90 painter2))))

(paint (below-1 einstein einstein))
(paint (below-2 einstein einstein))
(paint (another-below-2 einstein einstein))
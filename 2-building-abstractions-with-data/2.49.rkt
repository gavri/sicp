#lang sicp
(#%require sicp-pict)

(define (bottom-left frame)
  (frame-origin frame)
  )

(define (top-left frame)
  (vector-add (frame-origin frame) (frame-edge1 frame))
  )

(define (top-right frame)
  (vector-add (vector-add (frame-origin frame) (frame-edge1 frame)) (frame-edge2 frame))
  )

(define (bottom-right frame)
  (vector-add (frame-origin frame) (frame-edge2 frame))
  )

(define (outline-painter frame)
  (let ((top (make-segment (top-left frame) (top-right frame)))
        (right (make-segment (top-right frame) (bottom-right frame)))
        (bottom (make-segment (bottom-left frame) (bottom-right frame)))
        (left (make-segment (bottom-left frame) (top-left frame))))

    (segments->painter (list top right bottom left))))

(define (x-painter frame)
  (segments->painter (list (make-segment (top-right frame) (bottom-left frame)) (make-segment (top-left frame) (bottom-right frame)))))

(define (mean x y)
  (/ (+ x y) 2))

(define (mid-point start end)
  (make-vect (mean (vector-xcor start) (vector-xcor end)) (mean (vector-ycor start) (vector-ycor end)))
  )

(define (diamond-painter frame)
  (let ((left-mid-point (mid-point (bottom-left frame) (top-left frame)))
        (top-mid-point (mid-point (top-left frame) (top-right frame)))
        (right-mid-point (mid-point (top-right frame) (bottom-right frame)))
        (bottom-mid-point (mid-point (bottom-right frame) (bottom-left frame))))
    (segments->painter (list (make-segment left-mid-point top-mid-point)
                             (make-segment top-mid-point right-mid-point)
                             (make-segment right-mid-point bottom-mid-point)
                             (make-segment bottom-mid-point left-mid-point)))
    )
  )

(define (wave-painter frame)
  "no"
  )


(define the-frame (make-frame (make-vect 0 0) (make-vect 0 0.99) (make-vect 0.99 0)))
(define another-frame (make-frame (make-vect 0 0) (make-vect 0.15 0.45) (make-vect 0.45 0.15)))

(paint (outline-painter the-frame))
(paint (x-painter the-frame))
(paint (diamond-painter the-frame))
(paint (outline-painter another-frame))
(paint (x-painter another-frame))
(paint (diamond-painter another-frame))

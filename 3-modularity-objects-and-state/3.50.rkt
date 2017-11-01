#lang racket

(define (my-stream-map proc argstreams)
  (if (stream-empty? (car argstreams))
    empty-stream
    (stream-cons
      (apply proc (map stream-first argstreams))
      (apply my-stream-map
             (list proc (map stream-rest argstreams))))))

(require rackunit)

(define actual (my-stream-map + (list (stream* 10 20 empty-stream) (stream* 3 4 empty-stream))))

(check-equal? (stream-first actual) 13)
(check-equal? (stream-first (stream-rest actual)) 24)

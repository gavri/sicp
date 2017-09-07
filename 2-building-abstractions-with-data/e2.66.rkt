#lang racket

(define (make-record key attributes) (cons key attributes))
(define (key record) (car record))

(define (entry tree) (car tree))
(define (left-branch tree) (cadr tree))
(define (right-branch tree) (caddr tree))
(define (make-tree entry left right)
  (list entry left right))

(define (lookup given-key set-of-records)
  (cond ((null? set-of-records) null)
        ((eq? given-key (key (entry set-of-records))) (entry set-of-records))
        ((< given-key (key (entry set-of-records))) (lookup given-key (left-branch set-of-records)))
        (else (lookup given-key (right-branch set-of-records))))
  )

(require rackunit)

(check-equal?
  (lookup 3 (list (make-record 3 '(attr1 attr2)) '() '())) '(3 attr1 attr2))


(check-equal?
  (lookup 1 (list (make-record 3 '(attr1 attr2)) '() '())) null)


(check-equal?
  (lookup 2 (list (make-record 3 '(attr1 attr2)) (list (make-record 2 '(attr3 attr4)) '() '()) '())) '(2 attr3 attr4))



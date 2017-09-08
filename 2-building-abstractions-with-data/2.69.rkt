#lang racket

(include "huffman-tree.rkt")

(define (adjoin-set x set)
  (cond ((null? set) (list x))
        ((< (weight x) (weight (car set))) (cons x set))
        (else (cons (car set)
                    (adjoin-set x (cdr set))))))

(define (make-leaf-set pairs)
  (if (null? pairs)
    '()
    (let ((pair (car pairs)))
      (adjoin-set (make-leaf (car pair)
                             (cadr pair))
                  (make-leaf-set (cdr pairs))))))

(define (successive-merge nodes)
  (cond ((= 1 (length nodes)) (car nodes))
        (else (successive-merge (adjoin-set (make-code-tree (first nodes) (second nodes)) (cddr nodes))))))

(define (generate-huffman-tree pairs)
  (successive-merge (make-leaf-set pairs)))

(require rackunit)

(check-equal? (generate-huffman-tree '((a 10) (b 2) (c 3)))
              '(((leaf b 2) (leaf c 3) (b c) 5) (leaf a 10) (b c a) 15))

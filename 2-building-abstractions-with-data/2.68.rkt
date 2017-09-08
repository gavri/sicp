#lang racket

(include "huffman-tree.rkt")

(define (has-symbol? symbol tree)
  (member symbol (symbols tree))
  )

(define (encode-symbol symbol tree)
  (cond ((leaf? tree) '())
        ((has-symbol? symbol (left-branch tree)) (cons '0 (encode-symbol symbol (left-branch tree))))
        ((has-symbol? symbol (right-branch tree)) (cons '1 (encode-symbol symbol (right-branch tree))))
        (else (error (format "Tree doesn't have this symbol: ~a" symbol)))))

(define (encode message tree)
  (if (null? message)
    '()
    (append (encode-symbol (car message) tree)
            (encode (cdr message) tree))))

(require rackunit)

(define sample-message '(0 1 1 0 0 1 0 1 0 1 1 1 0))
(define sample-tree
  (make-code-tree (make-leaf 'A 4)
                  (make-code-tree
                    (make-leaf 'B 2)
                    (make-code-tree (make-leaf 'D 1)(make-leaf 'C 1)))))

(check-equal? (encode (decode sample-message sample-tree) sample-tree) sample-message)

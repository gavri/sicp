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


(define frequency '((a 2) (na 16) (boom 1) (sha 3) (get 2) (yip 9) (job 2) (wah 1)))
(define fifties-songs-huffman-tree (generate-huffman-tree frequency))
(define a-fifties-verse '(get a job sha na na na na na na na na get a job sha na na na na na na na na wah yip yip yip yip yip yip yip yip yip sha boom))

(define (log2 x) (/ (log x) (log 2)))

(check-equal? (length (encode a-fifties-verse fifties-songs-huffman-tree)) 84)
(check-equal? (* (length a-fifties-verse) (exact-ceiling (log2 (length frequency)))) 108)

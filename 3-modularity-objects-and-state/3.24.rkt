#lang racket
(require compatibility/mlist)
(require rackunit)

(define (make-table)
  (let ((local-table (mlist '*table*)))
    (define (lookup key-1 key-2)
      (let ((subtable (massoc key-1 (mcdr local-table))))
        (if subtable
          (let ((record (massoc key-2 (mcdr subtable))))(if record
                                                          (mcdr record)
                                                          false))
          false)))

    (define (insert! key-1 key-2 value)
      (let ((subtable (massoc key-1 (mcdr local-table))))
        (if subtable
          (let ((record (massoc key-2 (mcdr subtable))))
            (if record
              (set-mcdr! record value)
              (set-mcdr! subtable
                         (mcons (mcons key-2 value)
                                (mcdr subtable)))))
          (set-mcdr! local-table
                     (mcons (mlist key-1
                                   (mcons key-2 value))
                            (mcdr local-table)))))
      'ok)
    (define (dispatch m)
      (cond ((eq? m 'lookup-proc) lookup)
            ((eq? m 'insert-proc!) insert!)
            (else (error "Unknown operation -- TABLE" m))))
    dispatch))

(define table (make-table))

(check-equal? ((table 'insert-proc!) 'letters 'a 1) 'ok)
(check-equal? ((table 'insert-proc!) 'letters 'b 2) 'ok)
(check-equal? ((table 'insert-proc!) 'numbers '1 'one) 'ok)
(check-equal? ((table 'insert-proc!) 'numbers '2 'two) 'ok)

(check-equal? ((table 'lookup-proc) 'letters 'a) 1)
(check-equal? ((table 'lookup-proc) 'letters 'b) 2)
(check-equal? ((table 'lookup-proc) 'numbers '1) 'one)
(check-equal? ((table 'lookup-proc) 'numbers '2) 'two)

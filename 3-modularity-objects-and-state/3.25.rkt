#lang racket
(require compatibility/mlist)
(require rackunit)

(define (make-table)
  (let ((local-table (mlist)))
  (define (insert! keys value)
    (let ((record (massoc keys local-table)))
      (if record
      (set-mcdr! record value)
      (set! local-table (mcons (mcons keys value) local-table)))
    'ok
    ))

  (define (lookup keys)
    (let ((record (massoc keys local-table)))
      (if record
        (mcdr record)
        false
        )
      ))

  (define (dispatch m)
    (cond ((eq? m 'lookup-proc) lookup)
          ((eq? m 'insert-proc!) insert!)
          (else (error "Unknown operation -- TABLE" m))))
  dispatch))

(define table (make-table))

(check-equal? ((table 'insert-proc!) '(a b) 1) 'ok)
(check-equal? ((table 'insert-proc!) '(a b c1) 2) 'ok)
(check-equal? ((table 'insert-proc!) '(a b c2) 3) 'ok)
(check-equal? ((table 'insert-proc!) '(a b c2 d) 4) 'ok)
(check-equal? ((table 'insert-proc!) '(e) 5) 'ok)

(check-equal? ((table 'lookup-proc) '(a b)) 1)
(check-equal? ((table 'lookup-proc) '(a b c1)) 2)
(check-equal? ((table 'lookup-proc) '(a b c2)) 3)
(check-equal? ((table 'lookup-proc) '(a b c2 d)) 4)
(check-equal? ((table 'lookup-proc) '(e)) 5)

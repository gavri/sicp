#lang racket

(require compatibility/mlist)

(define (enclosing-environment env) (cdr env))
(define (first-frame env) (car env))
(define the-empty-environment '())

(define (make-frame vars vals)
  (mcons 'frame (mmap (lambda (var val) (mcons var val)) (list->mlist vars) (list->mlist vals))))

(define (extend-environment vars vals base-env)
  (if (= (length vars) (length vals))
    (cons (make-frame vars vals) base-env)
    (if (< (length vars) (length vals))
      (error "Too many arguments supplied" vars vals)
      (error "Too few arguments supplied" vars vals))))

(define (all-bindings env) (flatten (map (compose mlist->list mcdr) env)))
(define (find-binding var env) (findf (lambda (e) (eq? var (mcar e))) (all-bindings env)))

(define (lookup-variable-value var env)
    (let ((binding (find-binding var env)))
      (cond (binding (mcdr binding))
        (else (error "Unbound variable" var)))))

(define (set-variable-value! var val env)
    (let ((binding (find-binding var env)))
      (cond (binding (set-mcdr! binding val))
        (else (error "Unbound variable" var)))))

(define (define-variable! var val env)
    (let ((binding (find-binding var (list (first-frame env)))))
      (cond (binding (set-mcdr! binding val))
        (else (add-binding-to-frame! var val (first-frame env))))))

(define (add-binding-to-frame! var val frame)
  (set-mcdr! frame (mcons (mcons var val) (mcdr frame))))

(require rackunit)

(define first (extend-environment (list 'a 'b 'c) (list 1 2 3) the-empty-environment))
(define second (extend-environment (list 'd) (list 4) first))

(check-equal? (lookup-variable-value 'b first) 2)
(check-equal? (lookup-variable-value 'd second) 4)
(check-exn exn:fail? (lambda () (lookup-variable-value 'd first)))
(check-not-exn (lambda () (lookup-variable-value 'd second)))

(define third (extend-environment (list 'a 'b) (list 1 2) the-empty-environment))
(set-variable-value! 'b 3 third)
(check-equal? (lookup-variable-value 'b third) 3)
(check-exn exn:fail? (lambda () (set-variable-value! 'c 4 third)))

(define fourth (extend-environment (list 'a 'b) (list 1 2) the-empty-environment))
(check-equal? (lookup-variable-value 'b fourth) 2)
(define-variable! 'b 3 fourth)
(check-equal? (lookup-variable-value 'b fourth) 3)
(check-not-exn (lambda () (define-variable! 'c 4 fourth)))
(check-equal? (lookup-variable-value 'c fourth) 4)
(check-not-exn (lambda () (define-variable! 'c 5 fourth)))
(check-equal? (lookup-variable-value 'c fourth) 5)
(define fifth (extend-environment '(d) '(6) fourth))
(define-variable! 'a 10 fifth)
(check-equal? (lookup-variable-value 'a fifth) 10)
(check-equal? (lookup-variable-value 'a fourth) 1)

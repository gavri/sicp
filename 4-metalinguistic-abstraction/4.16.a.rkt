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

(define (assigned? binding) (not (eq? (mcdr binding) '*unassigned*)))

(define (lookup-variable-value var env)
  (let ((binding (find-binding var env)))
    (cond ((and binding (assigned? binding))  (mcdr binding))
          (binding (error "Unassigned variable" var))
          (else (error "Unbound variable" var)))))


(require rackunit)

(define env (extend-environment '(a b) '(1 *unassigned*) the-empty-environment))

(check-equal? (lookup-variable-value 'a env) 1)
(check-exn exn:fail? (lambda () (lookup-variable-value 'b env)))

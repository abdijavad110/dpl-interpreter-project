#lang racket

(provide (all-defined-out))
(define empty-env
(lambda () (list `empty-env)))

(define extend-env
  (lambda (var val)
    (set! env (list `extend-env var val env))))

(define apply-env
  (lambda (search-var env)
    (cond
      ((eqv? (car env) `empty-env)
       (report-no-binding-found search-var))
      ((eqv? (car env) `extend-env)
       (let ((saved-var (cadr env))
             (saved-val (caddr env))
             (saved-env (cadddr env)))
         (if (eqv? search-var saved-var)
             saved-val
             (apply-env  search-var saved-env))))
      (else
       (report-invalid-env )))))


(define report-no-binding-found
  (lambda (search-var)
    (display  "No binding variable" )))

(define report-invalid-env
  (lambda ()
    (display  "Bad environment")))




(define env (empty-env))


;(extend-env `x 2 )
;(extend-env `y 3 )
;(set! env (list `extend-env `x 2 env))
;(apply-env `x env)





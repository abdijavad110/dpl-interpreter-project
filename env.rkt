#lang racket

(provide (all-defined-out))
(define empty-env
(lambda () (list `empty-env)))

(define extend-env
  (lambda (var val)
    (set! env (list `extend-env var val env))))

(define extend-saved-env
  (lambda (var val saved-env)
    (set! env (list `extend-env var val saved-env))))

(define reset-env-and-return-val
  (lambda (orig-env res)
    (begin
      (set! env orig-env)
      res)))

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
       (report-invalid-env)))))


(define report-no-binding-found
  (lambda (search-var)
    (begin
      (display  "No binding variable " )
      (display search-var)
      (display "\n"))))

(define report-invalid-env
  (lambda ()
    (display  "Bad environment")))




(define env (empty-env))


;(extend-env `x 2 )
;(extend-env `y 3 )
;(set! env (list `extend-env `x 2 env))
;(apply-env `x env)





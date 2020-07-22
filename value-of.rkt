#lang racket

(require "lexer.rkt")
(require "parser.rkt")
(require "env.rkt")

(require (except-in racket < > =)
         (rename-in racket [< old<] [> old>] [= old=] ))

;expressed values------------------------
(struct expval (value))
(struct our-list-expval expval ())
(struct bool-expval expval ())
(struct number-expval expval ())
(struct string-expval expval ())
(struct null-expval expval ())

;NEW----------------------------------------------------
(struct proc-expval expval ())
(struct procedure (vars body saved-env))
(define proc->expval
  (lambda (p) (cond
    [(procedure? p) (proc-expval p)]
    [else (raise-user-error p) (raise-user-error "Error")])))

;thunk-------------
(struct thunk (exp senv))

;-------------------------------------------------------

(define racket-list->expval
  (lambda (l)
    (cond
      [(list? l) (our-list-expval l)]
      [else (raise-user-error l) (raise-user-error "Error")])))

(define bool->expval
  (lambda (b)
    (cond
      [(boolean? b) (bool-expval b)]
      [else (raise-user-error "Error")])))

(define number->expval
  (lambda (i)
    (cond
      [(number? i) (number-expval i)]
      [else (raise-user-error "Error")])))

(define string->expval
  (lambda (s)
    (cond
      [(string? s) (string-expval s)]
      [else (raise-user-error "Error")])))

(define null->expval
  (lambda () (null-expval 'NULL)))

(define p-null?
  (lambda (p) (eqv? 'NULL p)))
;---------------------------------------

(define value-of-command
  (lambda (com x)
    ;(display env)
    ;(display "###\n")
    (cond
      [RETURN? RETVAL]
      [(unitcom-expr? com) (value-of-unitcom (unitcom-expr-ucom com) env)] ;(error (unitcom-expr-ucom com)) 
      [(multi-command-expr? com) (begin
                               (value-of-command (multi-command-expr-mcom com) env)
                               (value-of-command (multi-command-expr-ucom com) env))])))

(define value-of-unitcom
  (lambda (ucom x)
    (cond
    [(whilecom-expr? ucom) (value-of-while-expr (whilecom-expr-whileexpr ucom) env)]
    [(ifcom-expr? ucom) (value-of-if-expr (ifcom-expr-ifexpr ucom) env)]
    [(assigncom-expr? ucom) (value-of-assign (assigncom-expr-assignexpr ucom) env)]
    [(returncom-expr? ucom) (begin (define a (value-of-return-expr (returncom-expr-returnexpr ucom) env)) (set! RETURN? #t) (set! RETURN-VAL (- RETURN-VAL 1)) (if (old< RETURN-VAL 0)
                                                                                                                                                 (set! RETVAL (make-return-value a))
                                                                                                                                                 (set! RETVAL a)) RETVAL)])))

(define value-of-while-expr
  (lambda (whileexpr x)
    (cond
      [(while-expr? whileexpr)  (let ([condition (expval-value (value-of-expression (while-expr-exp whileexpr) env))])
                                        (if (if (number? condition) (not (old= 0 condition)) condition)
                                            (begin (value-of-command (while-expr-com whileexpr) env)
                                            (value-of-while-expr whileexpr env))
                                            `()))
                                    ]
      [else (raise-user-error "not a whilecom")])))     

(define value-of-if-expr
  (lambda (ifexpr x)
    (cond
      [(if-expr? ifexpr)  (let ([condition (expval-value (value-of-expression (if-expr-exp1 ifexpr) env))])
                                (if (if (number? condition) (not (old= 0 condition)) condition)
                                     (value-of-command (if-expr-com1 ifexpr) env)
                                       (value-of-command (if-expr-com2 ifexpr) env)))
                                    ]
      [else (raise-user-error "not a ifcom")])))

;NEW----------------------------------------------------
(define value-of-assign
    (lambda (ae x)
      (cond
        [(assign-exp-expr? ae)  (extend-env (assign-var ae) (value-of-expression (assign-exp-expr-exp ae) env))]
        [(assign-function-expr? ae) (extend-env (assign-var ae) (value-of-func-expr (assign-function-expr-f ae) env))]
        [(assign-call-expr? ae) (extend-env (assign-var ae) (value-of-call-expr (assign-call-expr-c ae) env))]
        [else (raise-user-error "not a assigncom")])))
;-------------------------------------------------------

;helpers------------------------------------------------
(define <
  (lambda (a b)
    (begin
    (cond [(expval? a) (set! a (expval-value a))])
    (cond [(expval? b) (set! b (expval-value b))])
    (cond
      [(or (null? a) (null? b)) #f]
      [(and (string? a) (string? b)) (str-cmp `l a b)]
      [(and (list? a) (list? b)) (raise-user-error "can not compare two lists")]
      [(list? a)  (if (null? (cdr a)) (< (expval-value (car a)) b) (and (< (expval-value (car a)) b) (< (cdr a) b)))]
      [(list? b)  (if (null? (cdr b)) (< a (expval-value (car b))) (and (< a (expval-value (car b))) (< a (cdr b))))]
      [(and (number? a) (number? b)) (old< a b)]
      [else (raise-user-error "invalid comparison")]
      ))))

(define >
  (lambda (a b)
    (begin
    (cond [(expval? a) (set! a (expval-value a))])
    (cond [(expval? b) (set! b (expval-value b))])
    (cond
      [(or (null? a) (null? b)) #f]
      [(and (string? a) (string? b)) (str-cmp `g a b)]
      [(and (list? a) (list? b)) (raise-user-error  "can not compare two lists")]
      [(list? a)  (if (null? (cdr a)) (> (expval-value (car a)) b) (and (> (expval-value (car a)) b) (> (cdr a) b)))]
      [(list? b)  (if (null? (cdr b)) (> a (expval-value (car b))) (and (> a (expval-value (car b))) (> a (cdr b))))]
      [(and (number? a) (number? b)) (old> a b)]
      [else (raise-user-error "invalid comparison")]
      ))))

(define =
  (lambda (a b)
    (begin
    (cond [(expval? a) (set! a (expval-value a))])
    (cond [(expval? b) (set! b (expval-value b))])
    (cond
      [(and (p-null? a) (p-null? b)) #t]
      [(and (null? a) (null? b)) #t]
      [(and (list? a) (list? b)) (if (old= (length a) (length b)) (and (= (expval-value (car a)) (expval-value (car b))) (= (cdr a) (cdr b))) #f)]
      [(or (null? a) (null? b)) #f]
      [(list? a)  (if (null? (cdr a)) (= (expval-value (car a)) b) (and (= (expval-value (car a)) b) (= (cdr a) b)))]
      [(list? b)  (if (null? (cdr b)) (= a (expval-value (car b))) (and (= a (expval-value (car b))) (= a (cdr b))))]
      [(and (string? a) (string? b)) (equal? a b)]
      [(and (boolean? a) (boolean? b)) (not (xor a b))]
      [(and (number? a) (number? b)) (old= a b)]
      [else #f]
      ))))

(define !=
  (lambda (a b)
    (begin
    (cond [(expval? a) (set! a (expval-value a))])
    (cond [(expval? b) (set! b (expval-value b))])
    ;(display a)
    ;(display b)
    (cond
      [(and (p-null? a) (p-null? b)) #f]
      [(and (null? a) (null? b)) #f]
      [(and (string? a) (string? b)) (not (equal? a b))]
      [(and (boolean? a) (boolean? b)) (xor a b)]
      [(and (list? a) (list? b)) (if (old= (length a) (length b)) (or (!= (expval-value (car a)) (expval-value (car b))) (!= (cdr a) (cdr b))) #t)]
      [(or (null? a) (null? b)) #t]
      [(list? a)  (if (null? (cdr a)) (!= (expval-value (car a)) b) (or (!= (expval-value (car a)) b) (!=  (cdr a) b)))]
      [(list? b)  (if (null? (cdr b)) (!= a (expval-value (car b))) (or (!= a (expval-value (car b))) (!= a (cdr b))))]
      [(and (number? a) (number? b)) (not (old= a b))]
      [else #t]
      ))))

(define str-cmp-helper
    (lambda (f a b)
      (cond
       [(and (null? a) (null? b)) #f]
       [(and (null? a) (eqv? `l f)) #t]
       [(and (null? a) (eqv? `g f)) #f]
       [(and (null? b) (eqv? `l f)) #f]
       [(and (null? b) (eqv? `g f)) #t]
       [(eqv? f `l) (if (= (char->integer (car a)) (char->integer (car b))) (str-cmp-helper `l (cdr a) (cdr b)) (< (char->integer (car a)) (char->integer (car b))))]
       [(eqv? f `g) (if (= (char->integer (car a)) (char->integer (car b))) (str-cmp-helper `g (cdr a) (cdr b)) (> (char->integer (car a)) (char->integer (car b))))]
       [else (raise-user-error "it is not valid operation")])))

(define str-cmp
    (lambda (f a b)
      (begin
        (cond [(expval? a) (set! a (expval-value a))])
        (cond [(expval? b) (set! b (expval-value b))])
        (str-cmp-helper f (string->list a) (string->list b)))))

(define neg-helper
    (lambda (a)
      (begin
        (cond [(expval? a) (set! a (expval-value a))])
        (cond
            [(number? a) (number->expval (- a))]
            [(boolean? a) (bool->expval (not a))]
            [(list? a) (racket-list->expval (if (null? (cdr a)) (list (neg-helper (car a))) (cons (neg-helper (car a)) (expval-value (neg-helper (cdr a))))))]
            [else (raise-user-error "invalid argument after dash")]
        ))))

(define operator-helper
    (lambda (f a b)
      (begin
        ;(error a)
        ;(error b)
        (cond [(expval? a) (set! a (expval-value a))])
        (cond [(expval? b) (set! b (expval-value b))])
      (cond
        [(and (number? a) (number? b)) (if (and (equal? f /) (old= 0 b)) (raise-user-error "division by zero") (number->expval (f a b)))]
        [(and (boolean? a) (boolean? b)) (cond
                                           [(equal? f +) (bool->expval (or a b))]
                                           [(equal? f *) (bool->expval (and a b))]
                                           [else (raise-user-error "invalid operation between booleans")])]
        [(and (string? a) (string? b) (equal? f +)) (string->expval (string-append a b))]
        [(and (list? a) (list? b)) (our-list-expval (append a b))]
        [(list? a)  (if (null? (cdr a)) (our-list-expval (list (operator-helper f (car a) b))) (our-list-expval (cons (operator-helper f (car a) b) (let ([res (operator-helper f (cdr a) b)])
                                                                                                                                                      (if (expval? res) (expval-value res) (res))))))]
        [(list? b)  (if (null? (cdr b)) (our-list-expval (list (operator-helper f a (car b)))) (our-list-expval (cons (operator-helper f a (car b)) (let ([res (operator-helper f a (cdr b))])
                                                                                                                                                      (if (expval? res) (expval-value res) (res))))))]
        [else (raise-user-error "invalid arguments for operator")]))))

(define reference-helper
    (lambda (l idx)
      (cond
        [(expval? l) (set! l (expval-value l))])
      (if (null? (cdr idx)) (list-ref l (expval-value (car idx))) (reference-helper (list-ref l (expval-value (car idx))) (cdr idx)))));(list-ref (reverse (cdr (reverse idx))) (car (reverse idx))))))) ; check this line
;-------------------------------------------------------

(define value-of-return-expr
  (lambda (r x)
  (value-of-expression (return-expr-exp r) env)))

(define value-of-expression
  (lambda (e x)
    ;(display env)
    (cond
      [(aexp-expr? e) (value-of-aexpression (expression-a1 e) env)]
      [(greater?-expr? e) (bool->expval (>
                                         (expval-value (value-of-aexpression (expression-a1 e) env))
                                         (expval-value (value-of-aexpression (greater?-expr-a2 e) env))))]
      [(smaller?-expr? e) (bool->expval (<
                                         (expval-value (value-of-aexpression (expression-a1 e) env))
                                         (expval-value (value-of-aexpression (smaller?-expr-a2 e) env))))]
      [(equal?-expr? e) (bool->expval (=
                                         (expval-value (value-of-aexpression (expression-a1 e) env))
                                         (expval-value (value-of-aexpression (equal?-expr-a2 e) env))))]
      [(not-equal?-expr? e) (bool->expval (!=
                                         (expval-value (value-of-aexpression (expression-a1 e) env))
                                         (expval-value (value-of-aexpression (not-equal?-expr-a2 e) env))))])))

(define value-of-aexpression
  (lambda (a x)
  (cond
    [(bexp-expr? a) (value-of-bexpression (aexpression-b1 a) env)]
    [(minus-expr? a) (operator-helper -
                        (expval-value (value-of-bexpression (aexpression-b1 a) env))
                        (expval-value (value-of-aexpression (minus-expr-a1 a) env)))]
    [(plus-expr? a) (operator-helper +
                        (expval-value (value-of-bexpression (aexpression-b1 a) env))
                        (expval-value (value-of-aexpression (plus-expr-a1 a) env)))])))

(define value-of-bexpression
  (lambda (b x)
  (cond
    [(cexp-expr? b) (value-of-cexpression (bexpression-c1 b) env)]
    [(mult-expr? b) (let ([left (expval-value (value-of-cexpression (bexpression-c1 b) env))])
                        (case left
                          [(0) (number->expval left)]
                          [(#f) (bool->expval left)]
                          [else (operator-helper * left (expval-value (value-of-bexpression (mult-expr-b1 b) env)))]))]
    [(divide-expr? b) (operator-helper /
                                   (expval-value (value-of-cexpression (bexpression-c1 b) env))
                                   (expval-value (value-of-bexpression (divide-expr-b1 b) env)))])))

(define value-of-cexpression
  (lambda (c x)
    (cond
      [(neg-expr? c) (neg-helper (expval-value (value-of-cexpression (neg-expr-c1 c) env)))]
      [(par-expr? c) (value-of-expression (par-expr-c1 c) env)]
      [(posnum-expr? c) (number->expval (posnum-expr-posnumber c))]
      [(null-expr? c) (null->expval)]
      [(bool-expr? c) (bool->expval (bool-expr-val c))]
      [(var-expr? c) (apply-env (var-expr-var c) env)]
      [(string-expr? c) (string->expval (string-expr-string-val c))]
      [(list-expr? c) (value-of-our-list (list-expr-l c) env)]
      [(listmem-expr? c) (reference-helper (expval-value (apply-env (listmem-expr-var c) env)) (expval-value (value-of-listmem (listmem-expr-lm c) env)))])))

(define value-of-listValues
  (lambda (lv x)
    (cond
      [(val-exp-expr? lv) (list (value-of-expression (listValues-exp1 lv) env))]
      [(extended-listValues-expr? lv) (cons (value-of-expression (listValues-exp1 lv) env) (value-of-listValues (extended-listValues-expr-lv lv) env))])))

(define value-of-our-list
  (lambda (l x)
    (cond
      [(listValues-expr? l) (racket-list->expval (value-of-listValues (listValues-expr-lv l) env))]
      [(empty-list-expr? l) (racket-list->expval '())])))

(define value-of-listmem
  (lambda (lm x)
    (cond
      [(idx-expr? lm) (racket-list->expval (list (value-of-expression (listmem-exp1 lm) env)))]
      [(multi-idx-expr? lm) (racket-list->expval (cons (value-of-expression (listmem-exp1 lm) env)
                                                       (let ([tmp (value-of-listmem (multi-idx-expr-lm lm) env)])
                                                         (cond
                                                           [(expval? tmp) (expval-value tmp)]
                                                           [else (tmp)]))))])))

;New----------------------------------------------------
(define extend-env-args
  (lambda (vars args saved-env arg-env)
    (let (
        [var (vars-v vars)]
        [arg (args-exp1 args)])
      (begin
        ;(display saved-env)
        (define saved-env-copy saved-env)
        (if (and (multi-vars-expr? vars) (multi-args-expr? args))
            (begin
              (extend-env-args (multi-vars-expr-vs vars) (multi-args-expr-args args) saved-env-copy arg-env)
              (set! saved-env-copy env))
              ;(display saved-env-copy))
            (if (or (multi-vars-expr? vars) (multi-args-expr? args)) (raise-user-error "args number doesn't match variables") '()))
        ;(display saved-env-copy)
        ;(display "\n")
        ;(display arg-env)
        ;(display (expression-a1 arg))
        ;(display "###\n")
        (reset-env-and-return-val arg-env '())
        (extend-saved-env var (value-of-expression arg arg-env) saved-env-copy)))))

(define apply-procedure
  (lambda (rator args name)    
    (let (
        [vars (procedure-vars (expval-value rator))]
        [body (procedure-body (expval-value rator))]
        [saved-env (procedure-saved-env (expval-value rator))])
      (begin
        ;(display env)
        (define orig-env env)
        (define saved-env-copy saved-env)
        ;(display name)
        (set! saved-env-copy (list `extend-env name rator saved-env))
        ;(display saved-env-copy)
        ;(display orig-env)
        ;(display "#####\n")
        ;(display args)
        (let ([res (value-of-command body (extend-env-args vars args saved-env-copy orig-env))])
          (reset-env-and-return-val orig-env res))))  
      ))

(define value-of-func-expr
  (lambda (f env) (let (
      [vars (function-expr-vars f)]
      [body (function-expr-cmds f)])
    (proc->expval (procedure vars body env)))))

(define value-of-call-expr
  (lambda (c env)
    (begin
      ;(display "CALL")
      ;(display env)
      ;(display "#########\n")
    (let (
        [rator (apply-env (call-expr-v c) env)] ; fixme check accessing env
        [args (call-expr-args c)])
      (begin
        ;(display (procedure-saved-env (expval-value rator)))
        (set! RETURN-VAL (+ RETURN-VAL 1))
        (let ([r (apply-procedure rator args (call-expr-v c))])
        (begin
          (set! RETURN? #f)
          r)))
      ))))
;-------------------------------------------------------

;make return value--------------------------------------
(define make-return-value
  (lambda (x)
    (begin
    (cond
      [(and (expval? x) (not (null-expval? x))) (set! x (expval-value x))])
    (cond
      [(boolean? x) (if (equal? #t x) 'true 'false)]
      [(list? x) (if (null? x) '() (cons (make-return-value (car x)) (make-return-value (cdr x))))]
      [else x]))))
;-------------------------------------------------------


(define RETURN? #f)
(define RETURN-VAL 0)
(define RETVAL '())

(define evaluate
  (lambda (file_path)
    (begin
      (define lex-this (lambda (lexer input) (lambda () (lexer input))))
      (define my-lexer (lex-this our-lexer (open-input-file file_path)))
      (let ((parser-res (our-parser my-lexer))) (value-of-command parser-res env))
      )))

(evaluate "inp.txt")
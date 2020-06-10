#lang racket

(require "lexer.rkt")
(require "parser.rkt")
(require "env.rkt")

(require (except-in racket < > =)
         (rename-in racket [< old<] [> old>] [= old=] ))

(struct expval (value))
(struct our-list-expval expval ())
(struct bool-expval expval ())
(struct int-expval expval ())
(struct string-expval expval ())
(struct null-expval expval ())

(define racket-list->expval
  (lambda (l)
    (cond
      [(list? l) (our-list-expval l)]
      [else (display l) (display "Error")])))

(define bool->expval
  (lambda (b)
    (cond
      [(boolean? b) (bool-expval b)]
      [else (display "Error")])))

(define int->expval
  (lambda (i)
    (cond
      [(number? i) (int-expval i)]
      [else (display "Error")])))

(define string->expval
  (lambda (s)
    (cond
      [(string? s) (string-expval s)]
      [else (display "Error")])))

(define null->expval
  (lambda () (null-expval '())))

;########################################################################################################################################################################################################################################################################
(define value-of-command
  (lambda (com env)
    ;(display env)
    (cond
      [RETURN RETVAL]
      [(unitcom-expr? com) (value-of-unitcom (unitcom-expr-ucom com) env)] ;(display (unitcom-expr-ucom com)) 
      [(multi-command-expr? com) (begin
                               (value-of-command (multi-command-expr-mcom com) env)
                               (value-of-command (multi-command-expr-ucom com) env))])))

(define value-of-unitcom
  (lambda (ucom x)
    (cond
    [(whilecom-expr? ucom) (value-of-while-expr (whilecom-expr-whileexpr ucom) env)]
    [(ifcom-expr? ucom) (value-of-if-expr (ifcom-expr-ifexpr ucom) env)]
    [(assigncom-expr? ucom) (value-of-assign-expr (assigncom-expr-assignexpr ucom) env)]
    [(returncom-expr? ucom) (begin (define a (value-of-return-expr (returncom-expr-returnexpr ucom) env)) (set! RETVAL (make-return-value a)) (set! RETURN #t) RETVAL)])))

(define value-of-while-expr
  (lambda (whileexpr x)
    (cond
      [(while-expr? whileexpr)  (if (expval-value 
                                 (value-of-expression (while-expr-exp whileexpr)  env))
                                    (begin (value-of-command (while-expr-com whileexpr) env)
                                       (value-of-while-expr whileexpr env))
                                    `())
                                    ]
      [else (display "not a whilecom")])))     

(define value-of-if-expr
  (lambda (ifexpr x)
    (cond
      [(if-expr? ifexpr)  (if (expval-value 
                                 (value-of-expression (if-expr-exp1 ifexpr) env))
                                     (value-of-command (if-expr-com1 ifexpr) env)
                                       (value-of-command (if-expr-com2 ifexpr) env))
                                    ]
      [else (display "not a ifcom")])))

(define value-of-assign-expr
    (lambda (assignexpr x)
      (cond
        [(assign-expr? assignexpr)  (extend-env (assign-expr-var assignexpr) (value-of-expression (assign-expr-exp assignexpr) env))]                  
        [else (display "not a assigncom")])))

;########################################################################################################################################################################################################################################
; here we customize comparator functions:
(define <
  (lambda (a b)
    (begin
    (cond [(expval? a) (set! a (expval-value a))])
    (cond [(expval? b) (set! b (expval-value b))])
    (cond
      [(or (null? a) (null? b)) #f]
      [(and (string? a) (string? b)) (str-cmp `l a b)]  
      [(and (list? a) (list? b)) (display "can not compare two lists")]
      [(list? a)  (if (null? (cdr a)) (< (car a) b) (and (< (car a) b) (< (cdr a) b)))]
      [(list? b)  (if (null? (cdr b)) (< a (car b)) (and (< a (car b)) (< a (cdr b))))]
      [(and (number? a) (number? b)) (old< a b)]
      [else (display "invalid comparison")]
      ))))

(define >
  (lambda (a b)
    (begin
    (cond [(expval? a) (set! a (expval-value a))])
    (cond [(expval? b) (set! b (expval-value b))])
    (cond
      [(or (null? a) (null? b)) #f]
      [(and (string? a) (string? b)) (str-cmp `g a b)] 
      [(and (list? a) (list? b)) (display "can not compare two lists")]
      [(list? a)  (if (null? (cdr a)) (> (car a) b) (and (> (car a) b) (> (cdr a) b)))]
      [(list? b)  (if (null? (cdr b)) (> a (car b)) (and (> a (car b)) (> a (cdr b))))]
      [(and (number? a) (number? b)) (old> a b)]
      [else (display "invalid comparison")]
      ))))

(define =
  (lambda (a b)
    (begin
    (cond [(expval? a) (set! a (expval-value a))])
    (cond [(expval? b) (set! b (expval-value b))])
    (cond
      [(and (null? a) (null? b)) #t]
      [(and (list? a) (list? b)) (if (old= (length a) (length b)) (and (= (car a) (car b)) (= (cdr a) (cdr b))) #f)]
      [(list? a)  (if (null? (cdr a)) (= (expval-value (car a)) b) (and (= (expval-value (car a)) b) (= (cdr a) b)))]
      [(list? b)  (if (null? (cdr b)) (= a (expval-value (car b))) (and (= a (expval-value (car b))) (= a (cdr b))))]
      [(or (null? a) (null? b)) #f]
      [(and (string? a) (string? b)) (equal? a b)]
      [(and (boolean? a) (boolean? b)) (not (xor a b))]
;      [ (or (and (boolean? a) (number? b)) (and (boolean? b) (number? a))
;            (and (string? a) (number? b)) (and (string? b) (number? a))
;            (and (string? a) (boolean? b)) (and (string? b) (boolean? a)))
;        #f]
;      [else (old= a b)]
      [(and (number? a) (number? b)) (old= a b)]
      [else #f]
      )))) 

(define !=
  (lambda (a b)
    (begin
    (cond [(expval? a) (set! a (expval-value a))])
    (cond [(expval? b) (set! b (expval-value b))])
    (cond
      [(and (null? a) (null? b)) #f]
      [(and (string? a) (string? b)) (equal? a b)]
      [(and (boolean? a) (boolean? b)) (xor a b)]
      [(and (list? a) (list? b)) (if (old= (length a) (length b)) #t (and (!= (car a) (car b)) (!= (cdr a) (cdr b))))]
      [(list? a)  (if (null? (cdr a)) (!= (car a) b) (or (!= (car a) b) (!= (cdr a) b)))]
      [(list? b)  (if (null? (cdr b)) (!= a (car b)) (or (!= a (car b)) (!= a (cdr b))))]
      [(or (null? a) (null? b)) #f]
;      [ (or (and (boolean? a) (number? b)) (and (boolean? b) (number? a))
;            (and (string? a) (number? b)) (and (string? b) (number? a))
;            (and (string? a) (boolean? b)) (and (string? b) (boolean? a)))
;        #t]
;      [else (not (old= a b))]
      [(and (number? a) (number? b)) (not (old= a b))]
      [else #t]
      ))))

;###############################################################################################################################################################################################################################
;; helpers:
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
       [else (display "it is not valid operation")])))

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
        [(number? a) (int->expval (- a))]
        [(boolean? a) (bool->expval (not a))]
        [(list? a) (if (null? (cdr a)) (neg-helper (car a)) (cons (neg-helper (car a)) (neg-helper (cdr a))))]
        ;[else (display "invalid argument after dash")] ;TODO
        )))

(define operator-helper
    (lambda (f a b)
      (begin
        ;(display a)
        ;(display b)
        (cond [(expval? a) (set! a (expval-value a))])
        (cond [(expval? b) (set! b (expval-value b))])
      (cond
        [(and (number? a) (number? b)) (int->expval (f a b))]
        [(and (boolean? a) (boolean? b)) (cond
                                           [(equal? f +) (bool->expval (or a b))]
                                           [(equal? f *) (bool->expval (and a b))]
                                           [else (display "invalid operation between booleans")])]
        [(and (string? a) (string? b) (equal? f +)) (string->expval (string-append a b))]
        [(and (list? a) (list? b)) (our-list-expval (append a b))]
        [(list? a)  (if (null? (cdr a)) (our-list-expval (list (operator-helper f (car a) b))) (our-list-expval (cons (operator-helper f (car a) b) (let ([res (operator-helper f (cdr a) b)])
                                                                                                                                                      (if (expval? res) (expval-value res) (res))))))]
        [(list? b)  (if (null? (cdr b)) (our-list-expval (list (operator-helper f a (car b)))) (our-list-expval (cons (operator-helper f a (car b)) (let ([res (operator-helper f a (cdr b))])
                                                                                                                                                      (if (expval? res) (expval-value res) (res))))))]
        [else (display "invalid arguments for operator")]))))

(define reference-helper
    (lambda (l idx)
      (if (null? (cdr idx)) (list-ref l (expval-value (car idx))) (reference-helper (list-ref l (expval-value (car idx))) (cdr idx)))));(list-ref (reverse (cdr (reverse idx))) (car (reverse idx))))))) ; check this line

;################################################################################################################################################################################################################################
(define value-of-return-expr
  (lambda (r x)
  (value-of-expression (return-expr-exp r) env)))

(define value-of-expression
  (lambda (e x)
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
    [(mult-expr? b) (operator-helper *
                                   (expval-value (value-of-cexpression (bexpression-c1 b) env))
                                   (expval-value (value-of-bexpression (mult-expr-b1 b) env)))]
    [(divide-expr? b) (operator-helper /
                                   (expval-value (value-of-cexpression (bexpression-c1 b) env))
                                   (expval-value (value-of-bexpression (divide-expr-b1 b) env)))])))

(define value-of-cexpression
  (lambda (c x)
    (cond
      [(neg-expr? c) (neg-helper (expval-value (value-of-cexpression (neg-expr-c1 c) env)))]
      [(par-expr? c) (value-of-expression (par-expr-c1 c) env)]
      [(posnum-expr? c) (int->expval (posnum-expr-posnumber c))]
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
      [(multi-idx-expr? lm) (racket-list->expval (cons (value-of-expression (listmem-exp1 lm) env) (value-of-listmem (multi-idx-expr-lm lm) env)))])))
;########################################################################################################################################################################################################################

(define make-return-value
  (lambda (x)
    (begin
    (cond
      [(expval? x) (set! x (expval-value x))])
    (cond
      [(list? x) (if (null? x) '() (cons (make-return-value (car x)) (make-return-value (cdr x))))]
      [else x]))))
      ;TODO NULL
;########################################################################################################################################################################################################################




;test
;(define lex-this (lambda (lexer input) (lambda () (lexer input))))
;(define my-lexer (lex-this our-lexer (open-input-string "x = [1, 2, 3, 4, 5]; y = [1, 2, 3, 4, 5]; return x; if x != y then a = 4*[6, 10, 2]/2 else a = [17, 10, 2] endif; while a[1] > 1 do b = a[0]-1; a=[b, a[1]-1, 10, 2] end; return b")))
;)))
;"a = 2; while a < 4 do a = a + 1; b = 3 end; return a"
;(my-lexer)(my-lexer)(my-lexer)(my-lexer)(my-lexer)(my-lexer)(my-lexer)(my-lexer)(my-lexer)(my-lexer)(my-lexer)(my-lexer)(my-lexer)(my-lexer)(my-lexer)(my-lexer)(my-lexer)(my-lexer)
;(define env (empty-env))
(define RETURN #f)
(define RETVAL '())
;(let ((parser-res (our-parser my-lexer))) (value-of-command parser-res env)) ;(parse-object-to-list parser-res))
;(cdr (list 1))

(define evaluate
  (lambda (file_path)
    (begin
      (define lex-this (lambda (lexer input) (lambda () (lexer input))))
      (define my-lexer (lex-this our-lexer (open-input-file file_path)))
      (let ((parser-res (our-parser my-lexer))) (value-of-command parser-res env))
      )))

(evaluate "inp.txt")
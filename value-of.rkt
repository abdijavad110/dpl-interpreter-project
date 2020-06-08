#lang racket


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
      [else (display "Error")])))

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


(define value-of-command
  lambda (com env)
  (cond
    [(unitcom-expr? com) (value-of-unitcom (unitcom-expr-ucom com) env)]
    [(multi-command-expr? com) (begin
                               (value-of-command (multi-command-expr-mcom com) env)
                               (value-of-unitcom (multi-command-expr-ucom com) env))]))

(define value-of-unitcom
  lambda (ucom env)
    (cond
    [(whilecom-expr? ucom) (value-of-while-expr (whilecom-expr-whileexpr ucom) env)]
    [(ifcom-expr? ucom) (value-of-if-expr (ifcom-expr-ifexpr ucom) env)]
    [(assigncom-expr? ucom) (value-of-assign-expr (assigncom-expr-assignexpr ucom) env)]
    [(returncom-expr? ucom) (begin (value-of-return-expr (returncom-expr-returnexpr ucom) env) (exit))]))

(define value-of-while-expr
  (lambda (whileexpr env)
    (cond
      [(while-expr? whileexpr)  (if (bool-expval-value 
                                 (value-of-expression (while-expr-exp whileexpr)  env))
                                    (begin (value-of-command (while-expr-com whileexpr) env)
                                       (value-of-while-expr whileexpr env))
                                    `())
                                    ]
      [else (display "not a whilecom")])))



     

(define value-of-if-expr
  (lambda (ifexpr env)
    (cond
      [(if-expr? ifexpr)  (if (bool-expval-value 
                                 (value-of-expression (if-expr-exp1 ifexpr) env))
                                     (value-of-command (if-expr-com1 ifexpr) env)
                                       (value-of-command (if-expr-com2 ifexpr) env))
                                    ]
      [else (display "not a ifcom")])))



(define value-of-assign-expr
    (lambda (assignexpr env)
    (cond
      [(assign-expr? assignexpr)  (extend-env (assign-expr-var assignexpr) (value-of-experession (assign-expr-exp assignexpr) env) env)]
                          
      [else (display "not a assigncom")])))



; here we customize comparator functions:

(define <
  (lambda (a b)
  (cond
    [(or (null? a) (null? b)) #f]
    [(and (string? a) (string? b)) (str-cmp `l a b)]  
    [(and (list? a) (list? b)) (display "can not compare two lists")]
    [(list? a)  (if (null? (cdr a)) (< (car a) b) (and (< (car a) b) (< (cdr a) b)))]
    [(list? b)  (if (null? (cdr b)) (< a (car b)) (and (< a (car b)) (< a (cdr b))))]
    [else (old< a b)]
      )))

(define >
  (lambda (a b)
  (cond
    [(or (null? a) (null? b)) #f]
    [(and (string? a) (string? b)) (str-cmp `g a b)] 
    [(and (list? a) (list? b)) (display "can not compare two lists")]
    [(list? a)  (if (null? (cdr a)) (> (car a) b) (and (> (car a) b) (> (cdr a) b)))]
    [(list? b)  (if (null? (cdr b)) (> a (car b)) (and (> a (car b)) (> a (cdr b))))]
    [else (old> a b)]
      )))

(define =
  (lambda (a b)
  (cond
    [(and (null? a) (null? b)) #t]
    [(and (list? a) (list? b)) (if (old= (length a) (length b)) (and (= (car a) (car b)) (= (cdr a) (cdr b))) #f)]
    [(list? a)  (if (null? (cdr a)) (= (car a) b) (and (= (car a) b) (= (cdr a) b)))]
    [(list? b)  (if (null? (cdr b)) (= a (car b)) (and (= a (car b)) (= a (cdr b))))]
    [(or (null? a) (null? b)) #f]
    [(and (string? a) (string? b)) (equal? a b)]
    [(and (boolean? a) (boolean? b)) (not (xor a b))]
    [ (or (and (boolean? a) (number? b)) (and (boolean? b) (number? a))
          (and (string? a) (number? b)) (and (string? b) (number? a))
          (and (string? a) (boolean? b)) (and (string? b) (boolean? a)))
      #f]
    [else (old= a b)]
      )))

(define !=
  (lambda (a b)
  (cond
    [(and (null? a) (null? b)) #f]
    [(and (string? a) (string? b)) (equal? a b)]
    [(and (boolean? a) (boolean? b)) (xor a b)]
    [(and (list? a) (list? b)) (if (old= (length a) (length b)) #t (and (!= (car a) (car b)) (!= (cdr a) (cdr b))))]
    [(list? a)  (if (null? (cdr a)) (!= (car a) b) (or (!= (car a) b) (!= (cdr a) b)))]
    [(list? b)  (if (null? (cdr b)) (!= a (car b)) (or (!= a (car b)) (!= a (cdr b))))]
    [(or (null? a) (null? b)) #f]
    [ (or (and (boolean? a) (number? b)) (and (boolean? b) (number? a))
          (and (string? a) (number? b)) (and (string? b) (number? a))
          (and (string? a) (boolean? b)) (and (string? b) (boolean? a)))
      #t]
    [else (not (old= a b))]
      )))


;; helpers:
(define str-cmp-helper
    (lambda (f a b)
      (cond
       [(and (null? a) (null? b)) #t]
       [(and (null? a) (eqv? `l f)) #t]
       [(and (null? a) (eqv? `g f)) #f]
       [(and (null? b) (eqv? `l f)) #f]
       [(and (null? b) (eqv? `g f)) #t]
       [(eqv? f `l) (if (= (char->integer (car a)) (char->integer (car b))) (str-cmp-helper `l (cdr a) (cdr b)) (< (char->integer (car a)) (char->integer (car b))))]
       [(eqv? f `g) (if (= (char->integer (car a)) (char->integer (car b))) (str-cmp-helper `g (cdr a) (cdr b)) (> (char->integer (car a)) (char->integer (car b))))]
       [else (display "it is not valid operation")])))


(define str-cmp
    (lambda (f a b)
      (str-cmp-helper f (string->list a) (string->list b))))


(define neg-helper
    (lambda (a)
     [(number? a) (int->expval (- a))]
     [(boolean? a) (bool->expval (not a))]
     [(list? a) (if (null? (cdr a)) (neg-helper (car a)) (cons (neg-helper (car a)) (neg-helper (cdr a))))]
     [else (display "invalid argument after -")]))

(define operator-helper
    (lambda (f a b)
     (cond
      [(and (number? a) (number? b)) (int->expval (f a b))]
      [(and (boolean? a) (boolean? b)) (cond
                                        [(equal? f +) (bool->expval (or a b))]
                                        [(equal? f *) (bool->expval (and a b))]
                                        [else (display "invalid operation between booleans")]))]
      [(and (string? a) (string? b) (equal? f +)) (string->expval (string-append a b))]
      [(and (list? a) (list? b)) (append a b)]
      [(list? a)  (if (null? (cdr a)) (f (car a) b) (cons (f (car a) b) (f (cdr a) b)))]
      [(list? b)  (if (null? (cdr b)) (f a (car b)) (cons (f a (car b)) (f a (cdr b))))]
      [else (display "invalid arguments for operator")])))

(define reference-helper
    (lambda (l idx)
     (if (null? (cdr l)) (list-ref l (car idx)) (list-ref (reverse (cdr (reverse idx))) (car (reverse idx)))))) ; check this line



(define value-of-return-expr
  (lambda (r env)
  (value-of-expression (return-expr-exp r) env)))

(define value-of-expression
  (lambda (e env)
  (cond
    [(aexp-expr? e) (value-of-aexpression (expression-a1 e) env)]
    [(greater?-expr? e) (bool->expval (>
                                       (expval-value (value-of-aexpression (expression-a1 e) env))
                                       (expval-value (value-of-aexpression (expression-a2 e) env))))]
    [(smaller?-expr? e) (bool->expval (<
                                       (expval-value (value-of-aexpression (expression-a1 e) env))
                                       (expval-value (value-of-aexpression (expression-a2 e) env))))]
    [(equal?-expr? e) (bool->expval (=
                                       (expval-value (value-of-aexpression (expression-a1 e) env))
                                       (expval-value (value-of-aexpression (expression-a2 e) env))))]
    [(not-equal?-expr? e) (bool->expval (!=
                                       (expval-value (value-of-aexpression (expression-a1 e) env))
                                       (expval-value (value-of-aexpression (expression-a2 e) env)))))])))

(define value-of-aexpression
  (lambda (a env)
  (cond
    [(bexp-expr? a) (value-of-bexpression (aexpression-b1 a) env)]
    [(minus-expr? a) (operator-helper -
                        (expval-value (value-of-bexpression (aexpression-b1 a) env))
                        (expval-value (value-of-aexpression (aexpression-a1 a) env)))]
    [(plus-expr? a) (operator-helper +
                        (expval-value (value-of-bexpression (aexpression-b1 a) env))
                        (expval-value (value-of-aexpression (aexpression-a1 a) env)))])))

(define value-of-bexpression
  (lambda (b env)
  (cond
    [(cexp-expr? b) (value-of-cexpression (bexpression-c1 b) env)]
    [(mult-expr? b) (operator-helper *
                                   (expval-value (value-of-cexpression (bexpression-c1 b) env))
                                   (expval-value (value-of-bexpression (bexpression-b1 b) env)))]
    [(divide-expr? b) (operator-helper /
                                   (expval-value (value-of-cexpression (bexpression-c1 b) env))
                                   (expval-value (value-of-bexpression (bexpression-b1 b) env)))])))

(define value-of-cexpression
  (lambda (c env)
    (cond
      [(neg-expr? c) (neg-helper (expval-value (value-of-cexpression (neg-expr-c1 c) env)))]
      [(par-expr? c) (value-of-expression (par-expr-c1 c) env)]
      [(posnum-expr? c) (int->expval (posnum-expr-posnumber c))]
      [(null-expr? c) (null->expval)]
      [(bool-expr? c) (bool->expval (bool-expr-val c))]
      [(var-expr? c) (apply-env env (var-expr-var c))]
      [(string-expr? c) (string->expval (string-expr-string-val c))]
      [(list-expr? c) (value-of-our-list (list-expr-l c) env)]
      [(listmem-expr? c) (reference-helper (apply-env env (listmem-expr-var c)) (expval-value (value-of-listmem listmem-expr-lm)))])))

(define value-of-listValues
  (lambda (lv env)
    (cond
      [(val-exp-expr? lv) (list (value-of-expression (listValues-exp1 lv) env))]
      [(extended-listValues-expr? lv) (racket-list->expval (cons (value-of-expression (listValues-exp1 lv) env) (value-of-listValues (extended-listValues-expr-lv lv) env)))])))

(define value-of-our-list
  (lambda (l env)
    (cond
      [(listValues-expr? l) (racket-list->expval (value-of-listValues (listValues-expr-lv l) env))]
      [(empty-list-expr? l) (racket-list->expval '())])))

(define value-of-listmem
  (lambda (lm env)
    (cond
      [(idx-expr? lm) (list (value-of-expression (listmem-exp1 lm) env))]
      [(multi-idx-expr? lm) (cons (value-of-expression (listmem-exp1 lm) env) (value-of-listmem (multi-idx-expr-lm lm) env))])))

;test
;(define lex-this (lambda (lexer input) (lambda () (lexer input))))
;(define my-lexer (lex-this our-lexer (open-input-string "if x[4] == 3 then a = 6 else a = [17, 1, 2] endif; while x == 4 do b = b - 1; a = 7 end")))
;(my-lexer)(my-lexer)(my-lexer)(my-lexer)(my-lexer)(my-lexer)(my-lexer)(my-lexer)(my-lexer)(my-lexer)(my-lexer)(my-lexer)(my-lexer)(my-lexer)(my-lexer)(my-lexer)(my-lexer)(my-lexer)
;(let ((parser-res (our-parser my-lexer))) (parse-object-to-list parser-res))

#lang racket


(require "parser.rkt")
(require "env.rkt")

(struct expval (value))
(struct our-list-expval expval ())
(struct bool-expval expval ())
(struct int-expval expval ())
(struct string-expval expval ())


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
    [(returncom-expr? ucom) (value-of-return-expr (returncom-expr-returnexpr ucom) env)]))




(define value-of-return-expr
  lambda (r env)
  (value-of-expression (return-expr-exp r) env))

(define value-of-expression  ;; fix these for non numbers
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
    [(not-equal?-expr? e) (bool->expval (not (=
                                       (expval-value (value-of-aexpression (expression-a1 e) env))
                                       (expval-value (value-of-aexpression (expression-a2 e) env)))))])))

(define value-of-aexpression  ;; fix these for non numbers
  (lambda (a env)
  (cond
    [(bexp-expr? a) (value-of-bexpression (aexpression-b1 a) env)]
    [(minus-expr? a) (int-> expval (-
                                    (expval-value (value-of-bexpression (aexpression-b1 a) env))
                                    (expval-value (value-of-aexpression (aexpression-a1 a) env))))]
    [(plus-expr? a) (int-> expval (+
                                    (expval-value (value-of-bexpression (aexpression-b1 a) env))
                                    (expval-value (value-of-aexpression (aexpression-a1 a) env))))])))

(define value-of-bexpression  ;; fix these for non numbers
  (lambda (b env)
  (cond
    [(cexp-expr? b) (value-of-cexpression (bexpression-c1 b) env)]
    [(mult-expr? b) (int-> expval (*
                                   (expval-value (value-of-cexpression (bexpression-c1 b) env))
                                   (expval-value (value-of-bexpression (bexpression-b1 b) env))))]
    [(divide-expr? b) (int-> expval (/
                                   (expval-value (value-of-cexpression (bexpression-c1 b) env))
                                   (expval-value (value-of-bexpression (bexpression-b1 b) env))))])))

(define value-of-cexpression
  (lambda (c env)
    (cond
      [(neg-expr? c) (int->expval (- (expval-value (value-of-cexpression (neg-expr-c1 c) env))))] ; fix this for non numbers
      [(par-expr? c) (value-of-expression (par-expr-c1 c) env)]
      [(posnum-expr? c) (int->expval (posnum-expr-posnumber c))]
      [(null-expr? c) ()] ; fix this
      [(bool-expr? c) (bool->expval (bool-expr-val c))]
      [(var-expr? c) ()] ; fix this
      [(string-expr? c) (string->expval (string-expr-string-val c))]
      [(list-expr? c) (value-of-our-list (list-expr-l c) env)]
      [(listmem-expr? c) ()]))) ; fix this

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

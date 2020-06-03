#lang racket


(require "parser.rkt")

(struct expval (value))
(struct our-list-expval expval ())


(define racket-list->expval
  (lambda (l)
    (cond
      [(list? l) (our-list-expval l)]
      [else (display "Error")])))

(define value-of-command)
(define value-of-unitcom)
(define value-of-while-expr)
(define value-of-if-expr)
(define value-of-assign-expr)
(define value-of-return-expr)
(define value-of-expression)
(define value-of-aexpression)
(define value-of-bexpression)
(define value-of-cexpression)

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

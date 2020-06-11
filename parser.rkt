#lang racket
(require parser-tools/lex
         (prefix-in re- parser-tools/lex-sre)
         parser-tools/yacc)
(provide (all-defined-out))
(require "lexer.rkt")

(struct command ())
(struct unitcom-expr command (ucom))
(struct multi-command-expr command (mcom ucom))

;(define-datatype command command?
;  (unitcom-expr (ucom unitcom?))
;  (multi-command-expr (mcom command?) (ucom unitcom?))
;  )

(struct unitcom ())
(struct whilecom-expr unitcom (whileexpr))
(struct ifcom-expr unitcom (ifexpr))
(struct assigncom-expr unitcom (assignexpr))
(struct returncom-expr unitcom (returnexpr))

;(define-datatype unitcom unitcom?
;  (whilecom-expr (while while-expr?))
;  (ifcom-expr (if if-expr?))
;  (assigncom-expr (assign assign-expr?))
;  (returncom-expr (return return-expr?))
;  )

(struct while-expr (exp com))

;(define-datatype while while-expr?
;  (while-expr (exp expression?) (com command?))
;  )

(struct if-expr (exp1 com1 com2))

;(define-datatype if if-expr?
;  (if-expr (exp1 expression?) (com1 command?) (com2 command?))
;  )

(struct assign-expr (var exp))

;(define-datatype assign assign-expr?
;  (assign-expr (var symbol?) (exp expression?)) ;TODO
;  )

(struct return-expr (exp))

;(define-datatype return return-expr?
;  (return-expr (exp expression?))
;  )

(struct expression (a1))
(struct aexp-expr expression ())
(struct greater?-expr expression (a2))
(struct smaller?-expr expression (a2))
(struct equal?-expr expression (a2))
(struct not-equal?-expr expression (a2))

;(define-datatype expression expression?
;  (aexp-expr (a1 aexpression?))
;  (greater?-expr (a1 aexpression?) (a2 aexpression?))
;  (smaller?-expr (a1 aexpression?) (a2 aexpression?))
;  (equal?-expr (a1 aexpression?) (a2 aexpression?))
;  (not-equal?-expr (a1 aexpression?) (a2 aexpression?))
;  )

(struct aexpression (b1))
(struct bexp-expr aexpression ())
(struct minus-expr aexpression (a1))
(struct plus-expr aexpression (a1))

;(define-datatype aexp aexpression?
;  (bexp-expr (b1 bexpression?))
;  (minus-expr (b1 bexpression?) (a1 aexpression?))
;  (plus-expr (b1 bexpression?) (a1 aexpression?))
;  )

(struct bexpression (c1))
(struct cexp-expr bexpression ())
(struct mult-expr bexpression (b1))
(struct divide-expr bexpression (b1))

;(define-datatype bexp bexpression?
;  (cexp-expr (c1 cexpression?))
;  (mult-expr (c1 cexpression?) (b1 bexpression?))
;  (divide-expr (c1 cexpression?) (b1 bexpression?))
;  )

(struct cexpression ())
(struct neg-expr cexpression (c1))
(struct par-expr cexpression (c1))
(struct posnum-expr cexpression (posnumber))
(struct null-expr cexpression ())
(struct bool-expr cexpression (val))
(struct var-expr cexpression (var))
(struct string-expr cexpression (string-val))
(struct list-expr cexpression (l))
(struct listmem-expr cexpression (var lm))

;(define-datatype cexp cexpression?
;  (neg-expr (c1 cexpression?))
;  (par-expr (c1 cexpression?))
;  (posnum-expr (pnum positive?)) ;TODO
;  (null-expr)
;  (bool-expr (b string?)) ;TODO
;  (var-expr (v symbol?)) ;TODO
;  (string-expr (s string?))
;  (list-expr (l list?))
;  (listmem-expr (v string?) (lm listmem?))
;  )

(struct our-list ())
(struct listValues-expr our-list (lv))
(struct empty-list-expr our-list ())
 
;(define-datatype list list?
;  (listValues-expr (lv listValues?))
;  (empty-list-expr)
;  )

(struct listValues (exp1))
(struct val-exp-expr listValues ())
(struct extended-listValues-expr listValues (lv))

;(define-datatype listValues listValues?
;  (val-exp-expr (exp expression?))
;  (extended-listValues-expr (exp expression?) (lv listValues?))
;  )

(struct listmem (exp1))
(struct idx-expr listmem ())
(struct multi-idx-expr listmem (lm))

;(define-datatype listmem listmem?
;  (idx-expr (exp expression?))
;  (multi-idx-expr (exp expression?) (lm listmem?))
;  )



(define our-parser
  (parser
   (start command)
   (end EOF)
   (error (lambda (tok-ok? tok-name tok-value) (if tok-ok?
                                    (raise-user-error "Parse Error: Can't parse" tok-name)
                                    (raise-user-error "Parse Error: invalid" tok-name))))
   (tokens a b)
   (precs (left - +))
   (debug "input.txt")
   (grammar
    (command 
         ((unitcom) (unitcom-expr $1))
         ((command SEMICOL unitcom) (multi-command-expr $1 (unitcom-expr $3))))
    (unitcom
         ((whilecom) (whilecom-expr $1))
         ((ifcom) (ifcom-expr $1))
         ((assign) (assigncom-expr $1))
         ((return)(returncom-expr $1)))
    (whilecom
         ((WHILE exp DO command END) (while-expr $2 $4)))
    (ifcom
         ((IF exp THEN command ELSE command ENDIF) (if-expr $2 $4 $6)))
    (assign
         ((VARIABLE = exp) (assign-expr $1 $3)))
    (return
         ((RETURN exp) (return-expr $2)))
    (exp
         ((aexp) (aexp-expr $1))
         ((aexp > aexp) (greater?-expr $1 $3))
         ((aexp < aexp) (smaller?-expr $1 $3))
         ((aexp == aexp) (equal?-expr $1 $3))
         ((aexp != aexp) (not-equal?-expr $1 $3)))
    (aexp
         ((bexp) (bexp-expr $1))
         ((bexp - aexp) (minus-expr $1 $3))
         ((bexp + aexp) (plus-expr $1 $3)))
    (bexp
         ((cexp) (cexp-expr $1))
         ((cexp * bexp) (mult-expr $1 $3))
         ((cexp / bexp) (divide-expr $1 $3)))
    (cexp
         ((- cexp) (neg-expr $2))
         ((LPAR exp RPAR) (par-expr $2))
         ((POS) (posnum-expr $1))
         ((NULL) (null-expr))
         ((VARIABLE) (var-expr $1))
         ((TRUE) (bool-expr #t))
         ((FALSE) (bool-expr #f))
         ((STRING) (string-expr $1))
         ((our-list) (list-expr $1))
         ((VARIABLE listmem) (listmem-expr $1 $2)))
    (our-list
         ((LBRACKET listValues RBRACKET) (listValues-expr $2))
         ((LBRACKET RBRACKET) (empty-list-expr)))
    (listValues
         ((exp) (val-exp-expr $1))
         ((exp COMMA listValues) (extended-listValues-expr $1 $3)))
    (listmem
         ((LBRACKET exp RBRACKET) (idx-expr $2))
         ((LBRACKET exp RBRACKET listmem) (multi-idx-expr $2 $4)))
    )))


(define parse-object-to-list
  (lambda (root-object)
    (cond
      [(unitcom-expr? root-object) (list "unitcom-expr" (parse-object-to-list (unitcom-expr-ucom root-object)))]
      [(multi-command-expr? root-object) (list "multi-command-expr" (parse-object-to-list (multi-command-expr-mcom root-object)) (parse-object-to-list (multi-command-expr-ucom root-object)))]

      [(whilecom-expr? root-object) (list "whilecom-expr" (parse-object-to-list (whilecom-expr-whileexpr root-object)))]
      [(ifcom-expr? root-object) (list "ifcom-expr" (parse-object-to-list (ifcom-expr-ifexpr root-object)))]
      [(assigncom-expr? root-object) (list "assigncom-expr" (parse-object-to-list (assigncom-expr-assignexpr root-object)))]
      [(returncom-expr? root-object) (list "returncom-expr" (parse-object-to-list (returncom-expr-returnexpr root-object)))]

      [(while-expr? root-object) (list "while-expr" (parse-object-to-list (while-expr-exp root-object)) (parse-object-to-list (while-expr-com root-object)))]
      [(if-expr? root-object) (list "if-expr" (parse-object-to-list (if-expr-exp1 root-object)) (parse-object-to-list (if-expr-com1 root-object)) (parse-object-to-list (if-expr-com2 root-object)))]
      [(assign-expr? root-object) (list "assign-expr" (parse-object-to-list (assign-expr-var root-object)) (parse-object-to-list (assign-expr-exp root-object)))]
      [(return-expr? root-object) (list "return-expr" (parse-object-to-list (return-expr-exp root-object)))]
      
      [(aexp-expr? root-object) (list "aexp-expr" (parse-object-to-list (expression-a1 root-object)))]
      [(greater?-expr? root-object) (list "greater?-expr" (parse-object-to-list (expression-a1 root-object)) (parse-object-to-list (greater?-expr-a2 root-object)))]
      [(smaller?-expr? root-object) (list "smaller?-expr" (parse-object-to-list (expression-a1 root-object)) (parse-object-to-list (smaller?-expr-a2 root-object)))]
      [(equal?-expr? root-object) (list "equal?-expr" (parse-object-to-list (expression-a1 root-object)) (parse-object-to-list (equal?-expr-a2 root-object)))]
      [(not-equal?-expr? root-object) (list "not-equal?-expr" (parse-object-to-list (expression-a1 root-object)) (parse-object-to-list (not-equal?-expr-a2 root-object)))]

      [(bexp-expr? root-object) (list "bexp-expr" (parse-object-to-list (aexpression-b1 root-object)))]
      [(minus-expr? root-object) (list "minus-expr" (parse-object-to-list (minus-expr-a1 root-object)))]
      [(plus-expr? root-object) (list "plus-expr" (parse-object-to-list (plus-expr-a1 root-object)))]

      [(cexp-expr? root-object) (list "cexp-expr" (parse-object-to-list (bexpression-c1 root-object)))]
      [(mult-expr? root-object) (list "mult-expr" (parse-object-to-list (bexpression-c1 root-object)) (parse-object-to-list (mult-expr-b1 root-object)))]
      [(divide-expr? root-object) (list "divide-expr" (parse-object-to-list (bexpression-c1 root-object)) (parse-object-to-list (divide-expr-b1 root-object)))]

      [(neg-expr? root-object) (list "neg-expr" (parse-object-to-list (neg-expr-c1 root-object)))]
      [(par-expr? root-object) (list "par-expr" (parse-object-to-list (par-expr-c1 root-object)))]
      [(posnum-expr? root-object) (list "posnum-expr" (parse-object-to-list (posnum-expr-posnumber root-object)))]
      [(null-expr? root-object) (list "null-expr")]
      [(bool-expr? root-object) (list "bool-expr" (parse-object-to-list (bool-expr-val root-object)))]
      [(var-expr? root-object) (list "var-expr" (parse-object-to-list (var-expr-var root-object)))]
      [(string-expr? root-object) (list "string-expr" (parse-object-to-list (string-expr-string-val root-object)))]
      [(list-expr? root-object) (list "list-expr" (parse-object-to-list (list-expr-l root-object)))]
      [(listmem-expr? root-object) (list "listmem-expr" (parse-object-to-list (listmem-expr-var root-object)) (parse-object-to-list (listmem-expr-lm root-object)))]

      [(listValues-expr? root-object) (list "listValues-expr" (parse-object-to-list (listValues-expr-lv root-object)))]
      [(empty-list-expr? root-object) (list "empty-list-expr")]

      [(val-exp-expr? root-object) (list "val-exp-expr" (parse-object-to-list (listValues-exp1 root-object)))]
      [(extended-listValues-expr? root-object) (list "extended-listValues-expr" (parse-object-to-list (listValues-exp1 root-object)) (parse-object-to-list (extended-listValues-expr-lv root-object)))]

      [(idx-expr? root-object) (list "idx-expr" (parse-object-to-list (listmem-exp1 root-object)))]
      [(multi-idx-expr? root-object) (list "multi-idx-expr" (parse-object-to-list (listmem-exp1 root-object)) (parse-object-to-list (multi-idx-expr-lm root-object)))]

      [else root-object]

      )))

#lang racket
require parser-tools/lex
         (prefix-in re- parser-tools/lex-sre)
         parser-tools/yacc)
(provide (all-defined-out))

(define our-parser
  (parser
   (start command)
   (end EOF)
   (error void)
   (tokens a b)
   (precs (left - +))
   (grammar
    (command 
         ((unitcom) (unitcom-expr $1))
         ((command SEMICOL unitcom) (command-expr $1 $3)))
    (unitcom
         ((whilecom) (whilecom-expr $1))
         ((ifcom) (ifcom-expr $1))
         ((assign) (assign-expr $1))
         ((return)(return-expr $1)))
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
         (() (null-expr))
         ((VARIABLE) (var-expr $1))
         ((TRUE) (bool-expr $1))
         ((FALSE) (bool-expr $1))
         ((STRING) (string-expr $1))
         ((list) (list-expr $1))
         ((VARIABLE listmem) (listmem-expr $1 $2)))
    (list
         ((LBRACKET listValues RBRACKET) (listValues-expr $2))
         ((LBRACKET RBRACKET) (empty-list-expr))
    (listValues
         ((exp) (exp-val $1))
         ((exp , listValues) (extended-listValues-expr $1 $3)))
    (listmem
         ((LBRACKET exp RBRACKET) (idx-expr $2))
         ((LBRACKET exp RBRACKET listmem) (multi-idx-expr $2 $4)))
    )))
   
#lang racket

(require parser-tools/lex
         (prefix-in : parser-tools/lex-sre)
         parser-tools/yacc)
(provide (all-defined-out))




(define our-lexer
  (lexer
   ("func" (token-FUNC))
   ("{" (token-LBRACE))
   ("}" (token-RBRACE))
   (whitespace (our-lexer input-port))
   ((eof) (token-EOF))
   ("while" (token-WHILE))
   ("do" (token-DO))
   ("end" (token-END))
   ("if" (token-IF))
   ("then" (token-THEN))
   ("else" (token-ELSE))
   ("endif" (token-ENDIF))
   ("return" (token-RETURN))
   ("null" (token-NULL))
   ("true" (token-TRUE))
   ("false" (token-FALSE))
   (
    (:: (:or (char-range #\a #\z) (char-range #\A #\Z))
             (:* (:or (char-range #\a #\z) (char-range #\A #\Z))))
    (token-VARIABLE (string->symbol lexeme)))
   (
    (:or (:+ (char-range #\0 #\9)) (:: (:+ (char-range #\0 #\9)) #\. (:+ (char-range #\0 #\9))))
    (token-POS (string->number lexeme)))
   (
    (:: #\" (complement (:: any-string "\"" any-string)) #\")
    (token-STRING (string-trim lexeme "\"")))
   ("," (token-COMMA))
   (";" (token-SEMICOL))
   ("=" (token-=))
   ("<" (token-<))
   (">" (token->))
   ("==" (token-==))
   ("!=" (token-!=))
   ("(" (token-LPAR))
   (")" (token-RPAR))
   ("[" (token-LBRACKET))
   ("]" (token-RBRACKET))
   ("+" (token-+))
   ("*" (token-*))
   ("-" (token--))
   ("/" (token-/))
   (any-char (raise-user-error "invalid syntax on" lexeme))
))

(define-tokens a (VARIABLE POS STRING operator))
(define-empty-tokens b (EOF SEMICOL COMMA = < > == != + - * / LPAR RPAR LBRACKET RBRACKET
    WHILE DO END IF THEN ELSE ENDIF RETURN NULL TRUE FALSE FUNC LBRACE RBRACE))

(define evaluate
    (lambda (path)
      (file->string path)))

;test
(define lex-this (lambda (lexer input) (lambda () (lexer input))))
;(define my-lexer (lex-this our-lexer (open-input-string "a=func(x) {return x}")))
;(let ((parser-res (our-parser my-lexer))) parser-res)


;(display "read")
;(define my-lexer (lex-this our-lexer (evaluate "a.txt")))

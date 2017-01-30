#lang racket

(require brag/support)
(require "grammar.rkt")
(require br-parser-tools/lex)
(require (prefix-in : br-parser-tools/lex-sre))

(define operators
  '(("+" ADD)
    ("-" SUB)
    ("*" MUL)
    ("/" DIV)
    ("^" POW)
    ("%" MOD)
    ("(" LPAREN)
    (")" RPAREN)
    ("{" LBRACKET)
    ("}" RBRACKET)
    ("[" LSQUARE)
    ("]" RSQUARE)
    ("<" LT)
    ("<=" LTEQ)
    (">" GT)
    (">=" GTEQ)
    ("==" EQEQ)
    ("~=" NEQ)
    ("," COMMA)
    ("..." VARIADIC)
    (".." CONCAT)
    ("." PERIOD)
    (";" SEMICOLON)
    ("#" LEN)
    ("=" EQ)))

(define operator-map
  (make-hash operators))

(define keywords
  (make-hash
   '(("if" IF)
     ("else" ELSE)
     ("elseif" ELSEIF)
     ("for" FOR)
     ("while" WHILE)
     ("repeat" REPEAT)
     ("until" UNTIL)
     ("in" IN)
     ("do" DO)
     ("then" THEN)
     ("end" END)
     ("break" BREAK)
     ("return" RETURN)
     ("function" FUNCTION)
     ("local" LOCAL)
     ("true" TRUE)
     ("false" FALSE)
     ("nil" NIL)
     ("and" AND)
     ("or" OR)
     ("not" NOT))))

(define (name? n)
  (define id-p (pregexp id-regexp))
  (or
   (regexp-match-exact? id-p (symbol->string n))
   (eq? n '...))) ;; FIXME: this is pretty ...... horrible?

(define (constant? x)
  (or
   (boolean? x)
   (number? x)
   (string? x)
   (char? x)))

(define (variable? x)
  (symbol? x))

(define (operator? x)
  (hash-has-key? operator-map x))

(define (keyword? x)
  (hash-has-key? keywords x))

(define id-regexp "(\\p{L}|\\_)+")

(define (tokenizer-thunk ip)
  (port-count-lines! ip)
  (define my-lexer
    (lexer-src-pos
     [(repetition 1 +inf.0 (:or alphabetic (char-set "_")))
      (cond
        [(hash-has-key? keywords lexeme)
         (token (car (hash-ref keywords lexeme)) lexeme)]
        [else
         (token 'VAR lexeme)])]
     [(:: (repetition 1 +inf.0 numeric) (:? ".") (repetition 0 +inf.0 numeric))
      (token 'NUM (string->number lexeme))]
     [(:: "\"" (:* (:- any-char "\"")) "\"")
      (token 'STR lexeme)]
     [(:or
       "(" ")" "{" "}" "[" "]"
       "+" "-" "*" "/" "%" "^" "="
       ">" ">=" "<" "<=" "==" "~="
       "#" "..." ".." ";" ",")
      (token (car (hash-ref operator-map lexeme)) lexeme)]
     [whitespace
      (token 'WHITESPACE lexeme #:skip? #t)]
     [(eof) (void)]))
  (define (next-token) (my-lexer ip))
  next-token)

(provide
 name?
 constant?
 variable?
 operator?
 keyword?
 tokenizer-thunk)
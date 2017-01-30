#lang racket

(require brag/support)
(require "grammar.rkt")
(require br-parser-tools/lex)

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

(define (expand-operators ops)
  (string-join
   (for/list ([op ops])
     (regexp-quote (car op)))
   "|"))

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
     [(repetition 1 +inf.0 alphabetic) (token 'VAR lexeme)]
     [numeric (token 'NUM lexeme)]
     [whitespace (token 'WHITESPACE lexeme #:skip? #t)]
     [(eof) (void)]))
  (define (next-token) (my-lexer ip))
  next-token)

(define (tokenize p)
  (define ex-ops (expand-operators operators))
  (define op-regexp (pregexp (format "\"[^\"]+\"|~a|\\p{N}+|~a" id-regexp ex-ops)))
  (for/list ([b-str (regexp-match* op-regexp p)])
    (define str (bytes->string/utf-8 b-str))
    (cond
      [(hash-has-key? operator-map str)
       (token (car (hash-ref operator-map str)) str)]
      [(hash-has-key? keywords str)
       (token (car (hash-ref keywords str)) str)]
      [else
       (let* ([n (string->number str)])
         (cond
           [(number? n) (token 'NUM n)]
           [else
            (cond
              [(string-prefix? str "\"") (token 'STR (string-trim str "\""))]
              [else (token 'VAR str)])]))])))

(provide
 name?
 constant?
 variable?
 operator?
 keyword?
 tokenizer-thunk)

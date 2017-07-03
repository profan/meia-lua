#lang racket

(require brag/support)
(require "grammar.rkt")
(require br-parser-tools/lex)
(require (prefix-in : br-parser-tools/lex-sre))

(require racket/hash)
(require racket/set)

(define (make-bimap l)
  (hash-union
   (for/hash ([i l])
     (values (car i) (cadr i)))
   (for/hash ([i l])
     (values (cadr i) (car i)))))

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
    (":" COLON)
    ("." PERIOD)
    (";" SEMICOLON)
    ("#" LEN)
    ("=" EQ)
    ("and" AND)
    ("or" OR)
    ("not" NOT)))

(define operator-set
  (for/set ([o operators])
    (cadr o)))

(define operator-map
  (make-bimap operators))

(define keywords
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
    ("nil" NIL)))

(define keywords-hash
  (make-bimap keywords))

(define keyword-set
  (for/set ([k keywords])
    (cadr k)))

(define id-regexp "(\\p{L}|\\_)+(\\p{L}|\\_|\\p{N})*")

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
  (set-member? operator-set x))

(define (keyword? x)
  (set-member? keyword-set x))

(define (tokenizer-thunk ip)
  (port-count-lines! ip)
  (define my-lexer
    (lexer-src-pos
     [(:or
       "(" ")" "{" "}" "[" "]"
       "+" "-" "*" "/" "%" "^" "="
       ">" ">=" "<" "<=" "==" "~="
       "#" "..." ".." ";" ","
       ":" "." "and" "or" "not")
      (let ([tkn (hash-ref operator-map lexeme)])
        (token tkn tkn))]
     [(:: (:or alphabetic (char-set "_"))
          (repetition 0 +inf.0 (:or alphabetic numeric (char-set "_"))))
      (cond
        [(hash-has-key? keywords-hash lexeme)
         (let ([tkn (hash-ref keywords-hash lexeme)])
             (token tkn tkn))]
        [else
         (token 'VAR (string->symbol lexeme))])]
     [(:: (repetition 1 +inf.0 numeric) (:? ".") (repetition 0 +inf.0 numeric))
      (token 'NUM (string->number lexeme))]
     [(:: "\"" (:* (:- any-char "\"")) "\"")
      (token 'STR lexeme)]
     [(:: "'" (:* (:- any-char "'")) "'")
      (token 'STR lexeme)]
     [(:: "--[[" (:* (:- any-string "--[[")) "]]")
      (token 'COMMENT lexeme #:skip? #t)]
     [(:: "--[=[" (:* (:- any-string "--[=[")) "]=]")
      (token 'COMMENT lexeme #:skip? #t)]
     [(:: "--[==[" (:* (:- any-string "--[==[")) "]==]")
      (token 'COMMENT lexeme #:skip? #t)]
     [(:: "--[===[" (:* (:- any-string "--[===[")) "]===]")
      (token 'COMMENT lexeme #:skip? #t)]
     [(:: "--[====[" (:* (:- any-string "--[====[")) "]====]")
      (token 'COMMENT lexeme #:skip? #t)]
     [(:: "--[=====[" (:* (:- any-string "--[=====[")) "]=====]")
      (token 'COMMENT lexeme #:skip? #t)]
     [(:: "--[======[" (:* (:- any-string "--[======[")) "]======]")
      (token 'COMMENT lexeme #:skip? #t)]
     [(:: "--[=======[" (:* (:- any-string "--[========[")) "]=======]")
      (token 'COMMENT lexeme #:skip? #t)]
     [(:: "--[========[" (:* (:- any-string "--[========[")) "]========]")
      (token 'COMMENT lexeme #:skip? #t)]
     [(:: "--" (repetition 1 +inf.0 (:~ "\n")))
      (token 'COMMENT lexeme #:skip? #t)]
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
 operator-map
 keywords-hash
 tokenizer-thunk)

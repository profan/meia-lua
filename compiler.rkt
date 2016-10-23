#lang nanopass

(require brag/support)
(require "grammar.rkt")
(require syntax/parse)

(define operators
  (make-hash
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
     ("..." VARIADIC)
     (".." CONCAT)
     (";" SEMICOLON)
     ("." PERIOD)
     ("," COMMA)
     ("#" LEN)
     ("=" EQ))))

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
  (for/fold ([str ""]) ([op ops])
    (string-append str (format "\\~a" op))))

(define id-regexp "(\\p{L}|\\_)+")

(define (tokenize p)
  (define ex-ops (expand-operators (hash-keys operators)))
  (for/list ([b-str (regexp-match* (pregexp (format "\"[^\"]+\"|~a|\\p{N}+|[~a]" id-regexp ex-ops)) p)])
    (define str (bytes->string/utf-8 b-str))
    (cond
      [(hash-has-key? operators str) (token (car (hash-ref operators str)) str)]
      [(hash-has-key? keywords str) (token (car (hash-ref keywords str)) str)]
      [else
       (let* ([n (string->number str)])
         (cond
           [(number? n) (token 'NUM n)]
           [else
            (cond
              [(string-prefix? str "\"") (token 'STR (string-trim str "\""))]
              [else (token 'VAR str)])]))])))

(define (name? n)
  (define id-p (pregexp id-regexp))
  (regexp-match-exact? id-p n))

(define (constant? x)
  (or (number? x)
      (string? x)
      (char? x)))

(define (variable? x)
  (symbol? x))

(define (operator? x)
  (hash-has-key? operators x))

(define (keyword? x)
  (hash-has-key? keywords x))

(define-language L0
  (terminals
   (name (n))
   (constant (c))
   (variable (x))
   (operator (o)))
  (Expr (e body)
        c ;; constant
        x ;; variable
        (binop o e0 e1))
  (Stmt (s body)
        (assign x e)
        (fn n body)
        (while e body)
        (Expr e)))

(define-language L1
  (extends L0)
  (Stmt (s body)
        (+
         (op-assign o x e))))

(define-pass lower-op-assign : L1 (ir) -> L0 ()
  (definitions)
  (Stmt : Stmt (ir) -> Stmt ()
        [(op-assign ,o ,x ,[e]) `(assign x (binop o x e))])
  (Stmt ir))

(language->s-expression L0)
(language->s-expression L1)

(define (debug-display-data str)
  (define tokens (tokenize (open-input-string str)))
  (define parsed-data (parse tokens))
  (define syntax-data (syntax->datum parsed-data))
  (pretty-print tokens)
  (pretty-print syntax-data))

(debug-display-data "for value, other_value in pairs({12, 24}) do thing = 32 end")
(debug-display-data "function test() return \"Hello, World!\" end")
(debug-display-data "function test() local some_stuff = {1, 2, 3, 4}; return #some_stuff end")
(debug-display-data "function hello_world() return false end")
(debug-display-data "function hello_world() return not -25 end")
(debug-display-data "local thing = 25 + 35")

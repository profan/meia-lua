#lang nanopass

(require brag/support)
(require "grammar.rkt")

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

(define (tokenize p)
  (define ex-ops (expand-operators (hash-keys operators)))
  (for/list ([b-str (regexp-match* (pregexp (format "\"[^\"]+\"|(\\p{L}|\\_)+|\\p{N}+|[~a]" ex-ops)) p)])
    (define str (bytes->string/utf-8 b-str))
    (cond
      [(hash-has-key? operators str) (token (car (hash-ref operators str)))]
      [(hash-has-key? keywords str) (token (car (hash-ref keywords str)))]
      [else
       (let* ([n (string->number str)])
         (cond
           [(number? n) (token 'NUM n)]
           [else
            (cond
              [(string-prefix? str "\"") (token 'STR (string-trim str "\""))]
              [else (token 'VAR str)])]))])))

(define (variable? x)
  (symbol? x))

(define (constant? x)
  (or (number? x)
      (string? x)
      (char? x)))

(define (operator? x)
  (hash-has-key? operators x))

(define (keyword? x)
  (hash-has-key? keywords x))

(define-language L0
  (terminals
   (constant (c))
   (variable (x))
   (operator (o)))
  (Expr (e body)
        c ;; constant
        x ;; variable
        (op o e0 e1))
  (Stmt (s body)
        (assign x e)))

(define-language L1
  (extends L0)
  (Expr (e body)
        (+
         (op-assign o x e))))

(define-pass lower-operations : L1 (ir) -> L0 ()
  (definitions)
  (Expr : Expr (ir) -> Stmt ()
        [(op-assign ,o ,x ,[e]) `(assign x (op o x e))])
  (Expr ir))

(language->s-expression L0)
(language->s-expression L1)

(tokenize (open-input-string "for value, other_value in pairs({12, 24}) do 25 = 32 end"))
(tokenize (open-input-string "function test() return \"Hello, World!\" end"))
(tokenize (open-input-string "function test() local some_stuff = {1, 2, 3, 4}; return #some_stuff end"))
(tokenize (open-input-string "function hello_world() return false end"))
(tokenize (open-input-string "function hello_world() return not -25 end"))
(tokenize (open-input-string "local thing = 25 + 35"))

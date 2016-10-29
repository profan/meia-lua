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

;; CST to AST transformer here

(define (cst-to-ast cst)
  (displayln cst)
  (syntax-parse cst
    [({~literal chunk} stmts ...)
     (for/list ([s (syntax->list #'(stmts ...))])
       (cst-to-ast s))]
    [({~literal stat} {~optional (~datum "local")} (~seq namelist ...) (~datum "=") ...) #t]
    [else #f]))

;; AST parsing follows

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
  (Stmt (s)
        (assign (x* ... x) (e* ... e))
        (fn n s* ... s)
        (while e s* ... s)
        (ret e)
        (s* ... s)
        e)
  (Expr (e)
        x ;; variable
        c ;; constant
        (unop o e0)
        (binop o e0 e1)
        (e* ... e)))

(define-parser parse-L0 L0)

(define-language L1
  (extends L0)
  (Stmt (s body)
        (+
         (op-assign o (x* ... x) e1))))

(define-parser parse-L1 L1)

(define-pass lower-op-assign : L1 (ir) -> L0 ()
  (definitions)
  (Stmt : Stmt (ir) -> Stmt ()
        [(op-assign ,o (,x* ... ,x) (,[e*] ... ,[e]))
         (begin
           (define ops
             (with-output-language (L0 Expr)
              (for/list ([lhs (cons x x*)] [rhs (cons e e*)])
                `(binop ,o ,lhs ,rhs))))
           `(assign (,x* ... ,x) (,(cdr ops) ... ,(car ops))))])
  (Stmt ir))

(language->s-expression L0)
(language->s-expression L1)

(define (debug-print-data str)
  (define tokens (tokenize (open-input-string str)))
  (define parsed-data (parse tokens))
  (define syntax-data (syntax->datum parsed-data))
  (pretty-print tokens)
  (pretty-print syntax-data))

(debug-print-data "for value, other_value in pairs({12, 24}) do thing = 32 end")
(debug-print-data "function test() return \"Hello, World!\" end")
(debug-print-data "function test() local some_stuff = {1, 2, 3, 4}; return #some_stuff end")
(debug-print-data "function hello_world() return false end")
(debug-print-data "function hello_world() return not -25 end")
(debug-print-data "local x, y, z = 16, 24, 32")
(debug-print-data "local thing = 25 + 35")

(define-pass generate-code : L0 (ir) -> * ()
  (definitions
    (define (format-list lst #:sep [sep ""])
      (string-join
       (reverse
        (map (λ (n)
               (cond
                 [(L0-Stmt? n) (Stmt n)]
                 [(L0-Expr? n) (Expr n)]
                 [else n])) lst)) sep)))
  (Expr : Expr(e) -> * ()
        [,x (~a x)]
        [,c (~a c)]
        [(unop ,o ,e) (format "~a~a" o (Expr e))]
        [(binop ,o ,e1 ,e2) (format "~a ~a ~a" (Expr e1) o (Expr e2))]
        [(,e* ... ,e) (format-list (cons e e*) #:sep ", ")])
  (Stmt : Stmt(ir) -> *()
        [(assign (,x* ... ,x) (,e* ... ,e))
         (begin (displayln (format "x's: ~s" (format-list (cons x x*))))
                (format "~a = ~a"
                        (format-list (cons x x*) #:sep ", ")
                        (format-list (cons e e*) #:sep ", ")))]
        [(fn ,n ,s* ... ,s) (format "function ~a () ~n ~a ~nend" n (format-list (cons s s*)))]
        [(while ,e ,s* ... ,s) (format "while ~a do ~n ~a ~nend" (Expr e) (format-list (cons s s*)))]
        [(ret ,e) (format "return ~a" (Expr e))]
        [(,s* ... ,s) (format "~a" (format-list (cons s s*) #:sep "\n"))]))

;; MANUAL AST FOR TESTING OK FUC
(parse-L1 'x)
(parse-L1 '25)
(parse-L1 '(assign x 10))
(parse-L1 '(op-assign "+" x 25))
(parse-L1 '(op-assign "+" x (binop "-" 35 25)))
(lower-op-assign (parse-L1 '(op-assign "+" (x y) (24 (binop "-" 35 25)))))
(parse-L1 '(fn "hello_world" (ret 32)))

;; codegen testing
(displayln
 (generate-code
  (lower-op-assign
   (parse-L1 '((assign x 0)
               (while true (op-assign "+" x (binop "*" 5 2))))))))

(displayln
 (generate-code
  (lower-op-assign
   (parse-L1 '((assign (x y) (0 0))
               (while true (op-assign "+" (x y) ((binop "*" 32 16) 48))))))))

;; cst to ast testing
(cst-to-ast (parse (tokenize (open-input-string "local x, y = 32, 32"))))

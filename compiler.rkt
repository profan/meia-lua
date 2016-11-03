#lang nanopass

(require brag/support)
(require "grammar.rkt")
(require syntax/parse)

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

(define id-regexp "(\\p{L}|\\_)+")

(define (tokenize p)
  (define ex-ops (expand-operators operators))
  (define op-regexp (pregexp (format "\"[^\"]+\"|~a|\\p{N}+|~a" id-regexp ex-ops)))
  (for/list ([b-str (regexp-match* op-regexp p)])
    (define str (bytes->string/utf-8 b-str))
    (cond
      [(hash-has-key? operator-map str) (token (car (hash-ref operator-map str)) str)]
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

(define (cst->ast cst)
  (with-output-language (L1 Stmt)
    (syntax-parse cst
      [({~literal chunk} stmts ...)
       (for/list ([s (syntax->list #'(stmts ...))])
         (cst->ast s))]
      [({~literal stat} {~datum "do"} block {~datum "end"})
       (begin
         (define bs (apply append (cst->ast #'block)))
         `(begin ,bs ...))]
      [({~literal stat} {~optional (~datum "local")}
        (~and ns ({~literal namelist} (~seq names ...)))
        (~datum "=")
        (~and es ({~literal explist} (~seq exprs ...))))
       (begin
         (define n (cst->ast #'ns))
         (define e (cst->ast #'es))
         (displayln e)
         `(assign (,(cdr n) ... ,(car n))
                  (,(cdr e) ... ,(car e))))]
      [({~literal functioncall} name ({~literal args} (~optional {~datum "("}) exprs (~optional {~datum ")"})))
       (begin
         (define fname (cst->ast #'name))
         (define fexprs (cst->ast #'exprs))
         (define fargs
           (cond
             [(list? fexprs) fexprs]
             [else (list fexprs)]))
         `(call ,fname ,fargs ...))]
      [({~literal parlist} names)
       (cst->ast #'names)]
      [({~literal namelist} (~seq names ...))
       (for/list ([n (syntax->list #'(names ...))]
                  #:when (not (eqv? (syntax->datum n) ",")))
         (string->symbol (syntax->datum n)))]
      [({~literal explist} exprs ...)
       (for/list ([e (syntax->list #'(exprs ...))]
                  #:when (not (eqv? (syntax->datum e) ",")))
         (cst->ast e))]
      [({~literal block} chunks ...)
       (for/list ([c (syntax->list #'(chunks ...))])
         (cst->ast c))]
      [({~literal laststat} terms ...)
       (cst->ast #'(terms ...))]
      [({~datum "return"} exprs)
       (begin
         (define es (cst->ast #'exprs))
         `(ret ,es ...))]
      [({~literal prefixexp} e)
       (cst->ast #'e)]
      [({~literal tableconstructor} {~datum "{"} (~optional fieldlist) {~datum "}"})
       `(table)]
      [({~literal var} v)
       (string->symbol (syntax->datum #'v))]
      [(exp e0 ({~literal binop} op) e1)
       `(binop ,(cst->ast #'op) ,(cst->ast #'e0) ,(cst->ast #'e1))]
      [(exp ({~literal unop} op) e)
       `(unop ,(cst->ast #'op) ,(cst->ast #'e))]
      [(exp e)
       (cst->ast #'e)]
      [({~literal stat} (~datum "function")
        ({~literal funcname} name)
        ({~literal funcbody} {~datum "("} names {~datum ")"} body {~datum "end"}))
       (begin
         (define fname (string->symbol (syntax->datum #'name)))
         (define fnargs (cst->ast #'names))
         (define stmts (apply append (cst->ast #'body)))
         `(fn ,fname (,fnargs ...) (begin ,stmts ...)))]
      [(es ...)
       (for/list ([e (syntax->list #'(es ...))])
         (cst->ast e))]
      [e
       (syntax->datum #'e)])))

;; AST parsing follows

(define (name? n)
  (define id-p (pregexp id-regexp))
  (regexp-match-exact? id-p (symbol->string n)))

(define (constant? x)
  (or (number? x)
      (string? x)
      (char? x)))

(define (variable? x)
  (symbol? x))

(define (operator? x)
  (hash-has-key? operator-map x))

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
        (fn n (n* ...) s)
        (while e s)
        (begin s* ...)
        (ret e* ...)
        e)
  (Expr (e)
        x ;; variable
        c ;; constant
        (call n e* ...)
        (access e n)
        (index e0 e1)
        (table e* ...)
        (unop o e0)
        (binop o e0 e1)
        (e* ... e)))

(define-parser parse-L0 L0)

(define-language L1
  (extends L0)
  (Stmt (s body)
        (+
         (op-assign o (x* ... x) e))))

(define-parser parse-L1 L1)

(define-pass lower-op-assign : L1 (ir) -> L0 ()
  (definitions)
  (Stmt : Stmt (ir) -> Stmt ()
        ;; turns x, y += 10,24 into x, y = x + 10, y + 24
        [(op-assign ,o (,x* ... ,x) (,[e*] ... ,[e]))
         (begin
           (define ops
             (with-output-language (L0 Expr)
              (for/list ([lhs (cons x x*)] [rhs (cons e e*)])
                `(binop ,o ,lhs ,rhs))))
           `(assign (,x* ... ,x) (,(cdr ops) ... ,(car ops))))]
        ;; TODO: forms the case for expressions like x, y += call()
        ;;  which here should become ...
        ;;  local tmp_x, tmp_y = call()
        ;;  x, y = x + tmp_x, tmp_y
        [(op-assign ,o (,x* ... ,x) ,[e])
         `(assign (,x* ... ,x) (,e))])
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
    (define (format-list e e* #:sep [sep ""])
      (define l
        (cond
          [(empty? e) e*]
          [else (cons e e*)]))
      (string-join
       (map (λ (n)
              (cond
                [(L0-Expr? n) (Expr n)]
                [(L0-Stmt? n) (Stmt n)]
                [else n])) l) sep)))
  (Expr : Expr(e) -> * ()
        [,x (~a x)]
        [,c
         (cond
           [(or (string? c) (char?  c)) (format "\"~a\"" c)]
           [(number? c) (~a c)])]
        [(call ,n ,e* ...)
         (format "~a(~a)" n (format-list '() e* #:sep ", "))]
        [(unop ,o ,e)
         (format "~a~a" o (Expr e))]
        [(binop ,o ,e1 ,e2)
         (format "~a ~a ~a" (Expr e1) o (Expr e2))]
        [(table ,e* ...)
         (format "{~a}" (format-list '() e* #:sep ", "))]
        [(index ,e0 ,e1)
         (format "~a[~a]" (Expr e0) (Expr e1))]
        [(access ,e ,n)
         (format "~a.~a" (Expr e) n)]
        [(,e* ... ,e)
         (format-list e e* #:sep ", ")])
  (Stmt : Stmt(ir) -> *()
        [(assign (,x* ... ,x) (,e* ... ,e))
         (format "~a = ~a"
                 (format-list x x* #:sep ", ")
                 (format-list e e* #:sep ", "))]
        [(fn ,n (,n* ...) ,s)
         (format "function ~a (~a) ~n ~a ~nend" n (format-list '() n* #:sep ", ") (Stmt s))]
        [(while ,e ,s)
         (format "while ~a do ~n ~a ~nend" (Expr e) (Stmt s))]
        [(begin ,s* ...)
         (format "~a" (format-list '() s* #:sep "\n"))]
        [(ret ,e* ...)
         (format "return ~a" (format-list '() e* #:sep ", "))]))

;; MANUAL AST FOR TESTING OK FUC
(parse-L1 'x)
(parse-L1 '25)
(parse-L1 '(assign (x) (10)))
(parse-L1 '(assign (x y) (10 24)))
(parse-L1 '(op-assign "+" (x) (25)))
(lower-op-assign (parse-L1 '(op-assign "+" (x y z) (call print (25 32)))))
(parse-L1 '(op-assign "+" (x) (binop "-" 35 25)))
(lower-op-assign (parse-L1 '(op-assign "+" (x y) (24 (binop "-" 35 25)))))
(lower-op-assign (parse-L1 '(fn hello_world () (ret (32)))))
(displayln
 (generate-code (lower-op-assign (parse-L1 '(fn hello_world (a b c) (begin (ret (32))))))))

;; codegen testing
(displayln
 (generate-code
  (lower-op-assign
   (parse-L1 '(begin
                (assign (x) (0))
                (while true (op-assign "+" (x) ((binop "*" 5 2)))))))))

(displayln
 (generate-code
  (lower-op-assign
   (parse-L1 '(begin
                (assign (x y) (0 0))
                (while true (op-assign "+" (x y) ((binop "*" 32 16) 48)))
                (ret (32 24)))))))

(displayln
 (generate-code
  (lower-op-assign
   (parse-L1 '(begin
                (assign (t1 t2 t3) ((table (1 2 3 4)) (table (5 6 7 8)) (table)))
                (ret (t1 t2 t3)))))))

(displayln
 (generate-code
  (lower-op-assign
   (parse-L1 '(access love update)))))

(displayln
 (generate-code
  (lower-op-assign
   (parse-L1 '(index love 0)))))

;; cst to ast testing
(define test-syntax
  (parse
   (tokenize
    (open-input-string
     "do
        local x, y, z = 24, 32
        function hello_world(a, b, c)
          return a, b, c
        end
        local f, g, h = hello_world(x, y, z)
        local noparen = print \"hello, world\"
        local paren = print(\"hello, world\", 25, 32, {})
        local binopped = 25 + 32 * 42
        local unopped = -42
      end"))))

(pretty-print (syntax->datum test-syntax))
(pretty-print (parse-L1 '(assign (x y) (10 24))))
(pretty-print (car (cst->ast test-syntax)))
(displayln
 (generate-code
  (lower-op-assign
   (car (cst->ast test-syntax)))))

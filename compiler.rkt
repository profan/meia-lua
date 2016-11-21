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

;; CST to AST transformer here


(define-splicing-syntax-class cst/varlist
  (pattern ({~literal varlist} (~seq v:id ...))))

(define-splicing-syntax-class cst/namelist
  (pattern
   ({~literal namelist} (~seq n:id ...))))

(define-syntax-class cst/explist
  (pattern
   ((cst/expr (~optional {~datum ","})) ...)))

(define-syntax-class cst/functioncall
  (pattern
   (~or
    (cst/prefixexp cst/args)
    (cst/prefixexp {~datum ":"} var:id cst/args))))

(define-syntax-class cst/args
  (pattern
   (~or
    ({~datum "("} (~optional cst/explist) {~datum ")"})
    cst/tableconstructor
    str)))

(define-syntax-class cst/function
  (pattern
   ({~datum "function"} cst/funcbody)))

(define-syntax-class cst/funcbody
  (pattern
   ({~datum "("} (~optional cst/parlist) {~datum ")"} cst/block {~datum "end"})))

(define-syntax-class cst/prefixexp
  (pattern
   (~or
    cst/var
    cst/functioncall
    ({~datum "("} cst/expr {~datum ")"}))))

(define-syntax-class cst/tableconstructor
  (pattern
   ({~datum "{"} (~optional cst/fieldlist) {~datum "}"})))

(define-syntax-class cst/fieldlist
  (pattern
   ({~datum "{"} cst/field {~datum "}"})))

(define-syntax-class cst/field
  (pattern
   (~or
    ({~datum "{"} lhs:cst/expr {~datum "}"} {~datum "="} rhs:cst/expr)
    ({~literal var}))))

(define-syntax-class cst/binop
  (pattern thing))

(define-syntax-class cst/unop
  (pattern thing))

(define-syntax-class cst/expr
  (pattern
   (~and s
         (~or
          {~datum "nil"}
          {~datum "false"}
          {~datum "true"}
          {~datum "..."}))
   #:with expr (string->symbol (syntax->datum #'s)))
  (pattern
   cst/function)
  (pattern
   cst/prefixexp)
  (pattern
   (lhs:cst/expr op:cst/binop rhs:cst/expr)
   #:with expr #'(binop op.expr lhs.expr rhs.expr))
  (pattern
   (op:cst/unop e:cst/expr)
   #:with expr #'(unop op.expr e.expr)))

(define-syntax-class cst/var
  (pattern
   var:id)
  (pattern
   (pe:cst/prefixexp {~datum "{"} e:cst/expr {~datum "}"}))
  (pattern
   (pe:cst/prefixexp {~datum "."} var:id)))

(define-syntax-class cst/funcname
  (pattern
   (var:id (~seq {~datum "."} vs:id) (~optional ({~datum ":"} v:id)))))

(define-syntax-class cst/laststat
  (pattern
   (~or
    ({~datum "return"} (~optional cst/explist))
    {~datum "break"})))

(define-syntax-class cst/chunk
  (pattern
   ((~or cst/stat {~datum ";"}) ...)))

(define-syntax-class cst/block
  (pattern cst/chunk))

(define-syntax-class cst/stat
  (pattern
   (~or
    cst/functioncall
    ({~datum "do"} cst/block {~datum "end"})
    ({~datum "while"} cst/expr {~datum "do"} cst/block {~datum "end"})
    ({~datum "repeat"} cst/block {~datum "until"} cst/expr)
    ({~datum "if"} ife:cst/expr {~datum "then"} ifb:cst/block
     (~seq {~datum "elseif"} elseife:cst/expr {~datum "then"} elseifb:cst/block)
     (~optional ({~datum "else"} elseb:cst/block))
     {~datum "end"}))))

(define (cst->ast cst)
  (with-output-language (L1 Stmt)
    (syntax-parse cst
      [({~literal chunk} stmts ...)
       (for/list ([s (syntax->list #'(stmts ...))])
         (cst->ast s))]
      [({~literal stat} {~datum "do"} block {~datum "end"})
       (begin
         (define bs (cst->ast #'block))
         `(begin #t ,bs ...))]
      [({~literal stat}
        {~optional
         (~and local (~datum "local"))
         #:defaults ([local #'#f])}
        (~and ns
              (~or
               cst/namelist
               cst/varlist))
        (~datum "=")
        (~and es ({~literal explist} (~seq exprs ...))))
       (begin
         (define l (not (not (syntax->datum #'local))))
         (define n (cst->ast #'ns))
         (define e (cst->ast #'es))
         `(assign ,l
                  (,(cdr n) ... ,(car n))
                  (,(cdr e) ... ,(car e))))]
      [({~literal functioncall} name
        ({~literal args}
         (~optional {~datum "("})
         exprs
         (~optional {~datum ")"})))
       (begin
         (define fname (cst->ast #'name))
         (define fexprs (cst->ast #'exprs))
         (define fargs
           (cond
             [(list? fexprs) fexprs]
             [else (list fexprs)]))
         `(call ,fname ,fargs ...))]
      [({~literal parlist} names ...)
       (begin
         (define acc-ns
           (for/fold ([ns '()])
                     ([n (cst->ast #'(names ...))]
                      #:when (not (eqv? n ",")))
             (cond
               [(list? n) (append ns n)]
               [else (append ns (list n))])))
         acc-ns)]
      [({~literal varlist} (~seq vars ...))
       (for/list ([v (syntax->list #'(vars ...))])
         (cst->ast v))]
      [({~literal namelist} (~seq names ...))
       (for/list ([n (syntax->list #'(names ...))]
                  #:when (not (eqv? (syntax->datum n) ",")))
         (string->symbol (syntax->datum n)))]
      [({~literal explist} exprs ...)
       (for/list ([e (syntax->list #'(exprs ...))]
                  #:when (not (eqv? (syntax->datum e) ",")))
         (cst->ast e))]
      [({~literal block} chunks ...)
       (apply append (for/list ([c (syntax->list #'(chunks ...))])
          (cst->ast c)))]
      [({~literal laststat} terms ...)
       (cst->ast #'(terms ...))]
      [({~datum "return"} exprs)
       (begin
         (define es (cst->ast #'exprs))
         `(ret ,es ...))]
      [({~literal prefixexp} e)
       (cst->ast #'e)]
      [({~literal tableconstructor} {~datum "{"}
        (~optional fieldlist #:defaults ([fieldlist #'()]))
        {~datum "}"})
       `(table ,(cst->ast #'fieldlist) ...)]
      [({~literal fieldlist} fields ...)
       (for/list ([f (syntax->list #'(fields ...))]
                  #:when (not (eqv? (cst->ast f) ",")))
         (cst->ast f))]
      [({~literal var} v)
       (string->symbol (syntax->datum #'v))]
      [(exp e0 ({~literal binop} op) e1)
       `(binop ,(cst->ast #'op) ,(cst->ast #'e0) ,(cst->ast #'e1))]
      [(exp ({~literal unop} op) e)
       `(unop ,(cst->ast #'op) ,(cst->ast #'e))]
      [(exp e)
       (cst->ast #'e)]
      [({~literal stat}
        {~datum "if"}
        expr
        {~datum "then"}
        block
        (~optional {~datum "end"}))
       (begin
         (define e (cst->ast #'expr))
         (define body (cst->ast #'block))
         `(if ,e (begin #f ,body ...) #f))]
      [({~datum "elseif"}
        expr
        {~datum "then"}
        block
        (~optional {~datum "end"}))
       #f]
      [({~literal stat}
        {~datum "while"}
        expr
        {~datum "do"} block {~datum "end"})
       (begin
         (define body (cst->ast #'block))
         `(while ,(cst->ast #'expr) (begin #f ,body ...)))]
      [({~literal stat}
        {~datum "repeat"}
        block
        {~datum "until"}
        expr)
       (begin
         (define body (cst->ast #'block))
         (define test (cst->ast #'expr))
         `(repeat (begin #f ,body ...) ,test))]
      [({~literal stat}
        {~datum "for"}
        (~and ({~literal namelist} names ...) namelist)
        {~datum "in"}
        (~and {{~literal explist} exprs ...} explist)
        {~datum "do"}
        (~and ({~literal block} stmts ...) block)
        {~datum "end"})
       (begin
         (define ns (cst->ast #'namelist))
         (define es (first (cst->ast #'explist)))
         (define body (cst->ast #'block))
         `(for (,(cdr ns) ... ,(car ns)) ,es (begin #f ,body ...)))]
      [({~literal function} {~datum "function"}
        ({~literal funcbody}
         {~datum "("} names {~datum ")"}
         body
         {~datum "end"}))
      (begin
        (define fnargs (cst->ast #'names))
        (define stmts (cst->ast #'body))
        `(fn (,fnargs ...) (begin #f ,stmts ...)))]
      [({~literal stat}
        (~optional
         (~and local {~datum "local"})
         #:defaults ([local #'#f]))
        (~datum "function")
        (~or ({~literal funcname} name) name)
        ({~literal funcbody} {~datum "("} names {~datum ")"} body {~datum "end"}))
       (begin
         (define l (not (not (syntax->datum #'local))))
         (define fname (string->symbol (syntax->datum #'name)))
         (define fnargs (cst->ast #'names))
         (define stmts (cst->ast #'body))
         `(assign ,l (,fname) ((fn (,fnargs ...) (begin #f ,stmts ...)))))]
      [(es ...)
       (for/list ([e (syntax->list #'(es ...))])
         (cst->ast e))]
      [(~and s
        (~or
         {~datum "true"}
         {~datum "false"}
         {~datum "nil"}
         {~datum "..."}))
       (string->symbol (syntax->datum #'s))]
      [e
       (syntax->datum #'e)])))

;; AST parsing follows

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

(define-language Lua
  (terminals
   (name (n))
   (constant (c))
   (variable (x))
   (operator (o)))
  (Stmt (s)
        (assign c (x* ... x) (e* ... e))
        (while e s)
        (repeat s e)
        (if e s s?)
        (for (n* ... n) e s)
        (begin c s* ...)
        (ret e* ...)
        e)
  (Expr (e)
        x ;; variable
        c ;; constant
        (fn (n* ...) s)
        (call n e* ...)
        (access e n)
        (index e0 e1)
        (table e* ...)
        (unop o e0)
        (binop o e0 e1)
        (e* ... e)))

(define-parser parse-Lua Lua)

(define-language L1
  (extends Lua)
  (Stmt (s body)
        (+
         (op-assign c o (x* ... x) e))))

(define-parser parse-L1 L1)

(define-pass lower-op-assign : L1 (ir) -> Lua ()
  (definitions)
  (Stmt : Stmt (ir) -> Stmt ()
        ;; turns x, y += 10,24 into x, y = x + 10, y + 24
        [(op-assign ,c ,o (,x* ... ,x) (,[e*] ... ,[e]))
         (begin
           (define ops
             (with-output-language (Lua Expr)
              (for/list ([lhs (cons x x*)] [rhs (cons e e*)])
                `(binop ,o ,lhs ,rhs))))
           `(assign ,c (,x* ... ,x) (,(cdr ops) ... ,(car ops))))]
        ;; TODO: forms the case for expressions like x, y += call()
        ;;  which here should become ...
        ;;  local tmp_x, tmp_y = call()
        ;;  x, y = x + tmp_x, y + tmp_y
        [(op-assign ,c ,o (,x* ... ,x) ,[e])
         `(assign ,c (,x* ... ,x) (,e))])
  (Stmt ir))

(language->s-expression Lua)
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

(define-pass generate-code : Lua (ir) -> * ()
  (definitions
    (define (format-list e e* #:sep [sep ""])
      (define l
        (cond
          [(empty? e) e*]
          [else (cons e e*)]))
      (string-join
       (map (λ (n)
              (cond
                [(Lua-Expr? n) (Expr n)]
                [(Lua-Stmt? n) (Stmt n)]
                [else n])) l) sep)))
  (Expr : Expr(e) -> * ()
        [,x (~a x)]
        [,c
         (cond
           [(or (string? c) (char? c)) (format "\"~a\"" c)]
           [(number? c) (~a c)])]
        [(fn (,n* ...) ,s)
         (format "function (~a) ~n ~a ~nend" (format-list '() n* #:sep ", ") (Stmt s))]
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
        [(assign ,c (,x* ... ,x) (,e* ... ,e))
         (format "~a~a = ~a"
                 (if c "local " "")
                 (format-list x x* #:sep ", ")
                 (format-list e e* #:sep ", "))]
        [(if ,e ,s ,s?)
         (begin
           (define els
             (match s?
               [#f ""]
               [es (Stmt s?)]))
           (format "if ~a then ~n ~a ~a~nend" (Expr e) (Stmt s) els))]
        [(while ,e ,s)
         (format "while ~a do ~n ~a ~nend" (Expr e) (Stmt s))]
        [(repeat ,s ,e)
         (format "repeat ~n ~a ~nuntil ~a" (Stmt s) (Expr e))]
        [(for (,n* ... ,n) ,e ,s)
         (format "for ~a in ~a do ~n ~a ~nend" (format-list n n* #:sep ", ") (Expr e) (Stmt s))]
        [(begin ,c ,s* ...)
         (begin
           (define stmts (format "~a" (format-list '() s* #:sep "\n")))
           (if c (format "do ~n ~a ~nend" stmts) stmts))]
        [(ret ,e* ...)
         (format "return ~a" (format-list '() e* #:sep ", "))]))

;; MANUAL AST FOR TESTING OK FUC
(parse-L1 'x)
(parse-L1 '25)
(parse-L1 '(assign #t (x) (10)))
(parse-L1 '(assign #t (x y) (10 24)))
(parse-L1 '(op-assign #t "+" (x) (25)))
(lower-op-assign (parse-L1 '(op-assign #t "+" (x y z) (call print (25 32)))))
(parse-L1 '(op-assign #t "+" (x) (binop "-" 35 25)))
(lower-op-assign (parse-L1 '(op-assign #t "+" (x y) (24 (binop "-" 35 25)))))
(lower-op-assign (parse-L1 '(fn () (ret (32)))))
(displayln
 (generate-code (lower-op-assign (parse-L1 '(fn (a b c) (begin #f (ret (32))))))))

;; codegen testing
(displayln
 (generate-code
  (lower-op-assign
   (parse-L1 '(begin #t
                (assign #t (x) (0))
                (while true (op-assign #f "+" (x) ((binop "*" 5 2)))))))))

(displayln
 (generate-code
  (lower-op-assign
   (parse-L1 '(begin #t
                (assign #t (x y) (0 0))
                (while true (op-assign #f "+" (x y) ((binop "*" 32 16) 48)))
                (ret (32 24)))))))

(displayln
 (generate-code
  (lower-op-assign
   (parse-L1 '(begin #f
                (assign #t (t1 t2 t3) ((table (1 2 3 4)) (table (5 6 7 8)) (table)))
                (ret (t1 t2 t3)))))))

(displayln
 (generate-code
  (lower-op-assign
   (parse-L1 '(access love update)))))

(displayln
 (generate-code
  (lower-op-assign
   (parse-L1 '(index (index love 0) 1)))))

;; cst to ast testing
(define test-syntax
  (parse
   (tokenize
    (open-input-string
     "do
        local x, y, z = 24, 32
        local function hello_world(a, b, c)
          return a, b, c
        end
        local f, g, h = hello_world(x, y, z)
        local noparen = print \"hello, world\"
        local paren = print(\"hello, world\", 25, 32, {})
        local somefunc = function(a)
          return a * 2
        end
        x = 12
        function global_func(x, y, z)
          print \"global: hello world!\"
        end
        local some_table = {12, 24, 32, \"hello, world\", {true, {14, 24}}}
        for i, value in pairs(some_table) do
          thing = 32
          do
            thing = 64
          end
          print(i, value)
        end
        local lim = 0
        while lim < 10 do
          lim = lim + 1
        end
        local other_lim = 10
        repeat
          other_lim = other_lim + 1
          print(\"other_lim: \" .. other_lim)
        until other_lim == 10
        local binopped = 25 + 32 * 42
        local unopped = -42
        local function varfunc(a, ...)
          return ...
        end
        if true then
          world = true
        elseif false then
          world = false
        end
      end"))))

(pretty-print (syntax->datum test-syntax))
(pretty-print (parse-L1 '(assign #t (x y) (10 24))))
(pretty-print (car (cst->ast test-syntax)))
(displayln
 (generate-code
  (lower-op-assign
   (car (cst->ast test-syntax)))))

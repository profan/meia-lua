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

;; helper functions
(define (is-comma s)
  (syntax-parse s
    [{~datum ","} #t]
    [e #f]))

(define-splicing-syntax-class cst/varlist
  (pattern ({~literal varlist} (~seq vs:id ...))
           #:with expr
           (for/list ([id (syntax->list #'(vs ...))]
                      #:when (not (eqv? (syntax->datum id) ",")))
             (string->symbol (syntax->datum id)))))

(define-splicing-syntax-class cst/namelist
  (pattern
   ({~literal namelist} (~seq ns:id ...))
   #:with expr
   (for/list ([name (syntax->list #'(ns ...))]
              #:when (not (eqv? (syntax->datum name ","))))
     (string->symbol (syntax->datum name)))))

(define (extract-expr expr)
  (syntax-parse expr
    [e:cst/expr (attribute e.expr)]))

(define-syntax-class cst/explist
  (pattern
   (~and es ((cst/expr (~optional {~datum ","})) ...))
   #:with expr
   (for/list ([e (syntax-e #'es)]
              #:when (not (is-comma e)))
     (extract-expr e))))

(define-syntax-class cst/functioncall
  (pattern
   (cst/prefixexp {~datum ":"} var:id args:cst/args))
  (pattern
   (cst/prefixexp cst/args)))

(define-syntax-class cst/args
  (pattern
   ({~datum "("}
    (~optional
     elist:cst/explist
     #:defaults ([elist #'()]))
    {~datum ")"})
   #:with expr #'elist)
  (pattern
   (~or
    cst/tableconstructor
    str)))

(define-syntax-class cst/function
  (pattern
   ({~datum "function"} cst/funcbody)))

(define-syntax-class cst/funcbody
  (pattern
   ({~datum "("} (~optional args:cst/parlist) {~datum ")"} blk:cst/block {~datum "end"})))

(define-syntax-class cst/parlist
  (pattern
   (ns:cst/namelist (~optional ({~datum ","} {~datum "..."}))))
  (pattern
   {~datum "..."}))

(define-syntax-class cst/prefixexp
  (pattern
   var:cst/var)
  (pattern
   fn:cst/functioncall)
  (pattern
   ({~datum "("} cst/expr {~datum ")"})))

(define-syntax-class cst/tableconstructor
  (pattern
   ({~datum "{"} (~optional cst/fieldlist) {~datum "}"})))

(define-syntax-class cst/fieldlist
  (pattern
   ({~datum "{"} (~seq (~or cst/field {~datum ","})) {~datum "}"})))

(define-syntax-class cst/field
  (pattern
   ({~datum "{"} lhs:cst/expr {~datum "}"} {~datum "="} rhs:cst/expr))
  (pattern
   (lhs:id {~datum "="} rhs:cst/expr))
  (pattern
   ({~literal var})))

(define-syntax-class cst/binop
  (pattern
   (~or
    {~datum "+"}
    {~datum "-"}
    {~datum "*"}
    {~datum "/"}
    {~datum "%"}
    {~datum ".."}
    {~datum "<"}
    {~datum "<="}
    {~datum ">"}
    {~datum ">="}
    {~datum "=="}
    {~datum "~="}
    {~datum "and"}
    {~datum "or"})))

(define-syntax-class cst/unop
  (pattern
   (~or
    {~datum "-"}
    {~datum "not"}
    {~datum "#"})))

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
   (exp n:number)
   #:with expr #'n)
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
   (pe:cst/prefixexp {~datum "{"} e:cst/expr {~datum "}"})
   #:with expr #'(table e.expr))
  (pattern
   (pe:cst/prefixexp {~datum "."} v:id)
   #:with expr #'(access pe.expr v)))

(define-syntax-class cst/funcname
  (pattern
   (var:id (~seq {~datum "."} vs:id) (~optional ({~datum ":"} v:id)))))

(define-syntax-class cst/laststat
  (pattern
   ({~datum "return"} (~optional es:cst/explist))
   #:with expr #'(return es.expr))
  (pattern
   {~datum "break"}
   #:with expr #'(break)))

(define (extract-stmt s)
  (syntax-parse s
    [stmt:cst/stat (attribute stmt.expr)]))

(define-syntax-class cst/chunk
  (pattern
   ({~literal chunk}
    (~and stmts ((~or cst/stat {~datum ";"}) ...)))
   #:with expr
   (for/list ([s (syntax-e #'stmts)]
              #:when (not (is-comma s)))
     (extract-stmt s))))

(define-syntax-class cst/block
  (pattern cst/chunk))

(define-syntax-class cst/stat
  (pattern
   ({~literal stat}
    {~datum "local"}
    ns:cst/namelist {~datum "="} es:cst/explist)
   #:with expr #'(assign #t ns.expr es.expr))
  (pattern
   ({~literal stat}
    vs:cst/varlist {~datum "="} es:cst/explist)
   #:with expr #'(assign #f vs.expr es.expr))
  (pattern
   ({~datum "repeat"} blk:cst/block {~datum "until"} cnd:cst/expr)
   #:with expr #'(repeat blk.expr cnd.expr))
  (pattern
   ({~datum "do"} blk:cst/block {~datum "end"})
   #:with expr #'(begin #t (blk.expr)))
  (pattern
   ({~datum "while"} cnd:cst/expr {~datum "do"} blk:cst/block {~datum "end"})
   #:with expr #'(while cnd.expr blk.expr))
  (pattern
   (~or
    cst/functioncall
    ({~datum "if"} ife:cst/expr {~datum "then"} ifb:cst/block
     (~seq ({~datum "elseif"} eifes:cst/expr {~datum "then"} eifbs:cst/block))
     (~optional ({~datum "else"} elseb:cst/block))
     {~datum "end"}))
   #:with expr #'nil))

(define (new-cst->ast cst)
  (with-output-language (L1 Stmt)
    (displayln (format "cst: ~a" cst))
    (syntax->datum
     (syntax-parse cst
       [program:cst/chunk #'(program.expr)]))))

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
        (break)
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
         (format "return ~a" (format-list '() e* #:sep ", "))]
        [(break)
         (format "break")]))

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

(define new-test-syntax
  (parse
   (tokenize
    (open-input-string "local x, y, z = 12, 24, 32"))))

(pretty-print (syntax->datum new-test-syntax))
(pretty-print (parse-L1 '(assign #t (x y) (10 24))))
(pretty-print (car (new-cst->ast new-test-syntax)))
(displayln
 (generate-code
  (lower-op-assign
   (car (new-cst->ast new-test-syntax)))))

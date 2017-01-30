#lang nanopass

(require "lexer.rkt")
(require "grammar.rkt")
(require brag/support)
(require syntax/parse)

;; CST to AST transformer here

;; helper functions

(define-splicing-syntax-class cst/varlist
  (pattern
   ({~literal varlist} (~seq vs:id ...))
   #:with expr
   (for/list ([id (syntax->list #'(vs ...))]
              #:when (not (eqv? (syntax->datum id) ",")))
     (string->symbol (syntax->datum id)))))

(define-splicing-syntax-class cst/namelist
  (pattern
   ({~literal namelist} (~or {~datum ","} ns) ...)
   #:with expr
   (for/list ([name (syntax->list #'(ns ...))])
     (string->symbol (syntax->datum name)))))

(define (extract-expr expr)
  (syntax-parse expr
    [e:cst/expr (attribute e.expr)]))

(define-syntax-class cst/explist
  (pattern
   ({~literal explist} (~or {~datum ","} es:cst/expr) ...)
   #:with expr #'(es.expr ...)))

(define-syntax-class cst/functioncall
  (pattern
   ({~literal functioncall}
    cst/prefixexp {~datum ":"} var:id args:cst/args))
  (pattern
   ({~literal functioncall}
    cst/prefixexp cst/args)))

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
   ({~literal function}
    {~datum "function"} fn:cst/funcbody)
   #:with expr #'fn.expr))

(define-syntax-class cst/funcbody
  (pattern
   ({~literal funcbody}
    {~datum "("} (~optional args:cst/parlist) {~datum ")"} blk:cst/block {~datum "end"})
   #:with expr #'(fn args.expr (begin #f blk.expr))))

(define-syntax-class cst/parlist
  (pattern
   ({~literal parlist} {~datum "..."})
   #:with expr '(...))
  (pattern
   ({~literal parlist}
    ns:cst/namelist (~optional ({~datum ","} {~datum "..."})))
   #:with expr #'ns.expr))

(define-syntax-class cst/prefixexp
  (pattern
   ({~literal prefixexp}
    v:cst/var)
   #:with expr #'v.expr)
  (pattern
   ({~literal prefixexp}
    fn:cst/functioncall)
   #:with expr #'fn.expr)
  (pattern
   ({~literal prefixexp}
    {~datum "("} e:cst/expr {~datum ")"})
   #:with expr #'e.expr))

(define-syntax-class cst/tableconstructor
  (pattern
   ({~literal tableconstructor}
    {~datum "{"} (~optional fs:cst/fieldlist) {~datum "}"})
   #:with expr
   (if (attribute fs.expr)
       #'(table fs.expr)
       #'(table))))

(define-syntax-class cst/fieldlist
  (pattern
   ({~literal fieldlist}
    (~or fs:cst/field cst/fieldsep) ...)
   #:with expr #'(fs.expr ...)))

(define-syntax-class cst/field
  (pattern
   (field
    {~datum "["} lhs:cst/expr {~datum "]"} {~datum "="} rhs:cst/expr)
   #:with expr #'nil)
  (pattern
   (field
    lhs:id {~datum "="} rhs:cst/expr)
   #:with expr #'nil)
  (pattern
   (field
    e:cst/expr)
   #:with expr #'e.expr))

(define-syntax-class cst/fieldsep
  (pattern
   ({~literal fieldsep}
    (~or
     {~datum ","}
     {~datum ";"}))))

(define-syntax-class cst/binop
  (pattern
   ({~literal binop}
    (~and op
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
   #:with expr #'op))

(define-syntax-class cst/unop
  (pattern
   ({~literal unop}
    (~and op
          (~or
           {~datum "-"}
           {~datum "not"}
           {~datum "#"})))
   #:with expr (syntax->datum #'op)))

(define-syntax-class cst/expr
  (pattern
   (exp
    (~and s
          (~or
           {~datum "nil"}
           {~datum "false"}
           {~datum "true"}
           {~datum "..."})))
   #:with expr (string->symbol (syntax->datum #'s)))
  (pattern
   (exp n:number)
   #:with expr #'n)
  (pattern
   (exp fn:cst/function)
   #:with expr #'fn.expr)
  (pattern
   (exp lhs:cst/expr op:cst/binop rhs:cst/expr)
   #:with expr #'(binop op.expr lhs.expr rhs.expr))
  (pattern
   (exp pe:cst/prefixexp)
   #:with expr #'pe.expr)
  (pattern
   (exp tc:cst/tableconstructor)
   #:with expr #'tc.expr)
  (pattern
   (exp op:cst/unop e:cst/expr)
   #:with expr #'(unop op.expr e.expr)))

(define-syntax-class cst/var
  (pattern
   ({~literal var}
    v)
   #:with expr (string->symbol (syntax->datum #'v)))
  (pattern
   ({~literal var}
    pe:cst/prefixexp {~datum "["} e:cst/expr {~datum "]"})
   #:with expr #'(table e.expr))
  (pattern
   ({~literal var}
    pe:cst/prefixexp {~datum "."} v:id)
   #:with expr #'(access pe.expr v)))

(define-syntax-class cst/funcname
  (pattern
   ({~literal funcname}
    v ;(~seq {~datum "."} vs) (~optional ({~datum ":"} v))
    )
   #:with expr (string->symbol (syntax->datum #'v))))

(define-syntax-class cst/laststat
  (pattern
   ({~literal laststat}
    {~datum "return"} es:cst/explist)
   #:with expr #'(ret es.expr))
  (pattern
   ({~literal laststat}
    {~datum "return"})
   #:with expr #'(ret))
  (pattern
   ({~literal laststat}
    {~datum "break"})
   #:with expr #'(break)))

(define (extract-stmt s)
  (syntax-parse s
    [stmt:cst/stat (attribute stmt.expr)]))

(define-syntax-class cst/chunk
  (pattern
   ({~literal chunk}
    (~or stmts:cst/stat {~datum ";"}) ...
    (~optional
     (laststmt:cst/laststat (~optional {~datum ";"})) #:defaults ([laststmt #'()])))
   #:with expr
   (append
    (for/list ([s (syntax->list #'(stmts ...))])
      (extract-stmt s))
    #'()))
  (pattern
   ({~literal chunk}
    laststmt:cst/laststat (~optional {~datum ";"}))
   #:with expr #'(laststmt.expr))
  (pattern
   ({~literal chunk}
    (~or stmts:cst/stat {~datum ";"}) ...)
   #:with expr #'(stmts.expr ...)))

(define-syntax-class cst/block
  (pattern
   ({~literal block} chk:cst/chunk)
           #:with expr #'chk.expr))

(define-syntax-class cst/stat
  (pattern
   ({~literal stat}
    vs:cst/varlist {~datum "="} es:cst/explist)
   #:with expr #'(assign #f vs.expr es.expr))
  (pattern
   call:cst/functioncall
   #:with expr #'call.expr)
  (pattern
   ({~literal stat}
    {~datum "repeat"} blk:cst/block {~datum "until"} cnd:cst/expr)
   #:with expr #'(repeat blk.expr cnd.expr))
  (pattern
   ({~literal stat}
    {~datum "do"} blk:cst/block {~datum "end"})
   #:with expr #'(begin #t blk.expr))
  (pattern
   ({~literal stat}
    {~datum "while"} cnd:cst/expr {~datum "do"} blk:cst/block {~datum "end"})
   #:with expr #'(while cnd.expr (begin #f blk.expr)))
  (pattern
   ({~literal stat}
    {~datum "function"}
    fname:cst/funcname
    fnbody:cst/funcbody)
   #:with expr #'(assign #f (fname.expr) (fnbody.expr)))
  (pattern
   ({~literal stat}
    {~datum "local"}
    ns:cst/namelist {~datum "="} es:cst/explist)
   #:with expr #'(assign #t ns.expr es.expr))

  ;(pattern
   ;(~or
    ;cst/functioncall
    ;({~datum "if"} ife:cst/expr {~datum "then"} ifb:cst/block
    ; (~seq ({~datum "elseif"} eifes:cst/expr {~datum "then"} eifbs:cst/block))
    ; (~optional ({~datum "else"} elseb:cst/block))
    ; {~datum "end"}))
  ;#:with expr #'nil)
  )

(define (new-cst->ast cst)
  `(begin #f
          ,(syntax->datum
            (syntax-parse cst
              [program:cst/chunk #'program.expr]))))

;; AST parsing follows

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
        (begin c (s* ...))
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

; (language->s-expression Lua)
; (language->s-expression L1)

(define (debug-print-data str)
  (define token-thunk (tokenizer-thunk (open-input-string str)))
  (define parsed-data (parse token-thunk))
  (define syntax-data (syntax->datum parsed-data))
  (pretty-print (token-thunk))
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
          [else (append e* (list e))]))
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
        [(begin ,c (,s* ...))
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
(lower-op-assign (parse-L1 '(fn (x y z) (ret (32)))))
(displayln
 (generate-code (lower-op-assign (parse-L1 '(fn (a b c) (begin #f ((ret (32)))))))))

;; codegen testing
(displayln
 (generate-code
  (lower-op-assign
   (parse-L1 '(begin #t
                     ((assign #t (x) (0))
                      (while true (op-assign #f "+" (x) ((binop "*" 5 2))))))))))

(displayln
 (generate-code
  (lower-op-assign
   (parse-L1 '(begin #t
                     ((assign #t (x y) (0 0))
                      (while true (op-assign #f "+" (x y) ((binop "*" 32 16) 48)))
                      (ret (32 24))))))))

(displayln
 (generate-code
  (lower-op-assign
   (parse-L1 '(begin #f
                     ((assign #t (t1 t2 t3) ((table (1 2 3 4)) (table (5 6 7 8)) (table)))
                      (ret (t1 t2 t3))))))))

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
   (tokenizer-thunk
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
   (tokenizer-thunk
    (open-input-string
     "local x, y, z = 12, 24, 32
      local foo, bar = 10 + 24, 24 + 48
      local unary_foo, unary_bar = -foo, -bar
      local some_table = {12, 24, 48, {}, {25 / 32}}
      local what = false
      while what do
        local a, b, c = 1, 2, 3
      end
      local explicit_func = function(but, why)
        local thing, other_thing = but, why
      end
      function semicolon_things(a, b, c)
        local a = b; local b = c; local a = c;
      end
      function variadic_things(...)
        return 42
      end
      function does_things(a, b, c)
        return 24
      end"))))

(define (pretty-test name thunky)
  (displayln name)
  (pretty-print (thunky))
  (displayln ""))

(pretty-test "SYNTAX ->" (lambda () (syntax->datum new-test-syntax)))
(pretty-test "TEST 1 ->" (lambda () (parse-L1 '(assign #t (x y) (10 24)))))
(pretty-test "TEST 2 ->" (lambda () (new-cst->ast new-test-syntax)))
(pretty-test "TEST 3 ->" (lambda () (parse-L1 (new-cst->ast new-test-syntax))))
(pretty-test "TEST 4 ->" (lambda () (lower-op-assign (parse-L1 (new-cst->ast new-test-syntax)))))

(displayln
 (generate-code
  (lower-op-assign
   (parse-L1 (new-cst->ast new-test-syntax)))))

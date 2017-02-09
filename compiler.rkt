#lang nanopass

(require "lexer.rkt")
(require "grammar.rkt")
(require brag/support)
(require syntax/parse)

;; CST to AST transformer here

;; helper functions

(define-splicing-syntax-class cst/varlist
  (pattern
   ({~literal varlist} (~or vs:cst/var {~literal COMMA}) ...)
   #:with expr #'(vs.expr ...)))

(define-splicing-syntax-class cst/namelist
  (pattern
   ({~literal namelist} (~or {~literal COMMA} ns:id) ...)
   #:with expr #'(ns ...)))

(define-syntax-class cst/explist
  (pattern
   ({~literal explist} (~or {~literal COMMA} es:cst/expr) ...)
   #:with expr #'(es.expr ...)))

(define-syntax-class cst/functioncall
  (pattern
   ({~literal functioncall}
    pe:cst/prefixexp {~literal COLON} var:id args:cst/args)
   #:with expr #'(call (access #t pe.expr var) args.expr))
  (pattern
   ({~literal functioncall}
    pe:cst/prefixexp args:cst/args)
   #:with expr #'(call pe.expr args.expr)))

(define-syntax-class cst/args
  (pattern
   ({~literal args}
    {~literal LPAREN}
    (~optional elist:cst/explist)
    {~literal RPAREN})
   #:with expr
   (if (attribute elist.expr)
       #'elist.expr
       '("")))
  (pattern
   ({~literal args}
    (~and arg
          (~or
           tc:cst/tableconstructor
           s:str)))
   #:with expr
   (if (attribute tc.expr)
       #'tc.expr
       #'s)))

(define-syntax-class cst/function
  (pattern
   ({~literal function}
    {~literal FUNCTION} fn:cst/funcbody)
   #:with expr #'fn.expr))

(define-syntax-class cst/funcbody
  (pattern
   ({~literal funcbody}
    {~literal LPAREN} (~optional args:cst/parlist) {~literal RPAREN} blk:cst/block {~literal END})
   #:with expr
   (if (attribute args.expr)
       #'(fn args.expr (begin #f blk.expr))
       #'(fn () (begin #f blk.expr)))))

(define-syntax-class cst/parlist
  (pattern
   ({~literal parlist} {~literal VARIADIC})
   #:with expr '(...))
  (pattern
   ({~literal parlist}
    ns:cst/namelist
    (~optional (~seq {~literal COMMA} (~and v {~literal VARIADIC}))))
   #:with expr
   (begin
     (if (attribute v)
         (append
          (syntax-e #'ns.expr)
          (list '...))
         #'ns.expr))))

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
    {~literal LPAREN} e:cst/expr {~literal RPAREN})
   #:with expr #'(par e.expr)))

(define-syntax-class cst/tableconstructor
  (pattern
   ({~literal tableconstructor}
    {~literal LBRACKET} (~optional fs:cst/fieldlist) {~literal RBRACKET})
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
    {~literal LSQUARE} lhs:cst/expr {~literal RSQUARE} {~literal EQ} rhs:cst/expr)
   #:with expr #'(field #t lhs.expr rhs.expr))
  (pattern
   (field
    lhs:id {~literal EQ} rhs:cst/expr)
   #:with expr #'(field #f lhs rhs.expr))
  (pattern
   (field
    e:cst/expr)
   #:with expr #'e.expr))

(define-syntax-class cst/fieldsep
  (pattern
   ({~literal fieldsep}
    (~or
     {~literal COMMA}
     {~literal SEMICOLON}))))

(define-syntax-class cst/binop
  (pattern
   ({~literal binop}
    (~and op
          (~or
           {~literal ADD}
           {~literal SUB}
           {~literal MUL}
           {~literal DIV}
           {~literal MOD}
           {~literal CONCAT}
           {~literal LT}
           {~literal LTEQ}
           {~literal GT}
           {~literal GTEQ}
           {~literal EQEQ}
           {~literal NEQ}
           {~literal AND}
           {~literal OR})))
   #:with expr #'op))

(define-syntax-class cst/unop
  (pattern
   ({~literal unop}
    (~and op
          (~or
           {~literal SUB}
           {~literal NOT}
           {~literal LEN})))
   #:with expr #'op))

(define-syntax-class cst/expr
  (pattern
   (exp
    (~and s
          (~or
           {~literal NIL}
           {~literal FALSE}
           {~literal TRUE}
           {~literal VARIADIC})))
   #:with expr #'s)
  (pattern
   (exp n:number)
   #:with expr #'n)
  (pattern
   (exp s:str)
   #:with expr #'s)
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
    v:id)
   #:with expr #'v)
  (pattern
   ({~literal var}
    pe:cst/prefixexp {~literal LSQUARE} e:cst/expr {~literal RSQUARE})
   #:with expr #'(index pe.expr e.expr))
  (pattern
   ({~literal var}
    pe:cst/prefixexp {~literal PERIOD} v:id)
   #:with expr #'(access #f pe.expr v)))

(define-syntax-class cst/funcname
  (pattern
   ({~literal funcname}
    v1:id (~seq {~literal PERIOD} vs:id) ...
    (~optional (~seq {~literal COLON} v2:id)))
   #:with expr #'v1))

(define-syntax-class cst/laststat
  (pattern
   ({~literal laststat}
    {~literal RETURN} es:cst/explist)
   #:with expr #'(ret es.expr))
  (pattern
   ({~literal laststat}
    {~literal RETURN})
   #:with expr #'(ret))
  (pattern
   ({~literal laststat}
    {~literal BREAK})
   #:with expr #'(break)))

(define-syntax-class cst/chunk
  (pattern
   ({~literal chunk}
    laststmt:cst/laststat (~optional {~literal SEMICOLON}))
   #:with expr #'(laststmt.expr))
  (pattern
   ({~literal chunk}
    (~or stmts:cst/stat {~literal SEMICOLON}) ...
    (~optional (~seq laststmt:cst/laststat (~optional {~literal SEMICOLON}))))
   #:with expr
   (if (attribute laststmt.expr)
       #'(stmts.expr ... laststmt.expr)
       #'(stmts.expr ...))))

(define-syntax-class cst/block
  (pattern
   ({~literal block} chk:cst/chunk)
   #:with expr #'chk.expr))

(define-syntax-class cst/local
  (pattern
   {~literal LOCAL}
   #:with expr #'#t))

(define-splicing-syntax-class cst/elseif
  (pattern
   (~seq {~literal ELSEIF} e:cst/expr {~literal THEN} b:cst/block)
   #:with expr #'(if e.expr (begin #f b.expr) () #f #f)))

(define-syntax-class cst/stat
  (pattern
   ({~literal stat}
    vs:cst/varlist {~literal EQ} es:cst/explist)
   #:with expr #'(assign #f vs.expr es.expr))
  (pattern
   ({~literal stat}
    fncall:cst/functioncall)
   #:with expr #'fncall.expr)
  (pattern
   ({~literal stat}
    {~literal REPEAT} blk:cst/block {~literal UNTIL} cnd:cst/expr)
   #:with expr #'(repeat blk.expr cnd.expr))
  (pattern
   ({~literal stat}
    {~literal IF}
    e:cst/expr
    {~literal THEN}
    blk:cst/block
    eifs:cst/elseif ...
    (~optional (~seq {~literal ELSE} eblk:cst/block))
    {~literal END})
   #:with expr
   (cond
     [(and (attribute eifs.expr) (attribute eblk.expr))
      #'(if e.expr (begin #f blk.expr) (eifs.expr ...) (begin #f eblk.expr) #t)]
     [(attribute eblk.expr)
      #'(if e.expr (begin #f blk.expr) () (begin #f eblk.expr) #t)]
     [(attribute eifs.expr)
      #'(if e.expr (begin #f blk.expr) (eifs.expr ...) #f #t)]
     [else
      #'(if e.expr (begin #f blk.expr) () #f #t)]))
  (pattern
   ({~literal stat}
    {~literal FOR}
    v:id {~literal EQ}
    exp1:cst/expr {~literal COMMA} exp2:cst/expr
    (~optional (~seq {~literal COMMA} exp3:cst/expr))
    {~literal DO}
    blk:cst/block
    {~literal END})
   #:with expr
   (if (attribute exp3.expr)
       #'(for v exp1.expr exp2.expr exp3.expr (begin #f blk.expr))
       #'(for v exp1.expr exp2.expr 1 (begin #f blk.expr))))
  (pattern
   ({~literal stat}
    {~literal FOR}
    ns:cst/namelist
    {~literal IN}
    es:cst/explist
    {~literal DO}
    blk:cst/block
    {~literal END})
   #:with expr #'(for ns.expr es.expr (begin #f blk.expr)))
  (pattern
   ({~literal stat}
    {~literal DO} blk:cst/block {~literal END})
   #:with expr #'(begin #t blk.expr))
  (pattern
   ({~literal stat}
    {~literal WHILE} cnd:cst/expr {~literal DO} blk:cst/block {~literal END})
   #:with expr #'(while cnd.expr (begin #f blk.expr)))
  (pattern
   ({~literal stat}
    {~literal LOCAL}
    {~literal FUNCTION}
    fname:id
    fnbody:cst/funcbody)
   #:with expr #'(begin
                   #f
                   ((assign #t (fname) ())
                    (assign #f (fname) (fnbody.expr)))))
  ; this botch is just to match the function:thing() pattern, everything else mimics the grammar
  (pattern
   ({~literal stat}
    {~literal FUNCTION}
    ({~literal funcname} var:id {~literal COLON} mem:id)
    ({~literal funcbody}
     {~literal LPAREN} (~optional args:cst/parlist)
     (~bind [a
             (if (attribute args.expr)
                 (datum->syntax #f (append (list 'self) (syntax-e #'args.expr)))
                 #'(self))])
     {~literal RPAREN}
     blk:cst/block
     {~literal END}))
   #:with expr #'(assign #f ((access #f var mem)) ((fn a (begin #f blk.expr)))))
  (pattern
   ({~literal stat}
    {~literal FUNCTION}
    fname:cst/funcname
    fnbody:cst/funcbody)
   #:with expr #'(assign #f (fname.expr) (fnbody.expr)))
  (pattern
   ({~literal stat}
    {~literal LOCAL}
    ns:cst/namelist (~optional (~seq {~literal EQ} es:cst/explist)))
   #:with expr
   (if (attribute es.expr)
       #'(assign #t ns.expr es.expr)
       #'(assign #t ns.expr ()))))

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
        (assign c (e0* ... e0) (e1* ...))
        (while e s)
        (repeat s e)
        (if e s (s* ...) s? c?)
        (for n e0 e1 e2 s)
        (for (n* ... n) e s)
        (begin c (s* ...))
        (ret e* ...)
        (break)
        e)
  (Expr (e)
        x ;; variable
        c ;; constant
        (fn (n* ...) s)
        (call e e* ...)
        (access c e n)
        (index e0 e1)
        (field c e0 e1)
        (table e* ...)
        (unop o e0)
        (binop o e0 e1)
        (par e)
        (e* ... e)))

(define-parser parse-Lua Lua)

(define-language L1
  (extends Lua)
  (Stmt (s body)
        (+
         (op-assign c o (e0* ... e0) e))))

(define-parser parse-L1 L1)

(define-pass lower-op-assign : L1 (ir) -> Lua ()
  (definitions)
  (Stmt : Stmt (ir) -> Stmt ()
        ;; turns x, y += 10,24 into x, y = x + 10, y + 24
        [(op-assign ,c ,o (,e0* ... ,e0) (,[e1*] ...))
         (begin
           (define ops
             (with-output-language (Lua Expr)
               (for/list ([lhs (cons e0 e0*)] [rhs (cons e1* '())])
                `(binop ,o ,lhs ,rhs))))
           `(assign ,c (,e0* ... ,e0) (,(cdr ops) ... ,(car ops))))]
        ;; TODO: forms the case for expressions like x, y += call()
        ;;  which here should become ...
        ;;  local tmp_x, tmp_y = call()
        ;;  x, y = x + tmp_x, y + tmp_y
        [(op-assign ,c ,o (,e0* ... ,e0) ,[e])
         `(assign ,c (,e0* ... ,e0) (,e))])
  (Stmt ir))

; (language->s-expression Lua)
; (language->s-expression L1)

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
           [(or (string? c) (char? c)) (format "~a" c)]
           [(number? c) (~a c)])]
        [(fn (,n* ...) ,s)
         (format "function (~a) ~n ~aend" (format-list '() n* #:sep ", ") (Stmt s))]
        [(call ,e ,e* ...)
         (format "~a(~a)" (Expr e) (format-list '() e* #:sep ", "))]
        [(unop ,o ,e)
         (let ([op-str (hash-ref operator-map o)])
             (format "~a ~a" op-str (Expr e)))]
        [(binop ,o ,e1 ,e2)
         (let ([op-str (hash-ref operator-map o)])
             (format "~a ~a ~a" (Expr e1) op-str (Expr e2)))]
        [(table ,e* ...)
         (format "{~a}" (format-list '() e* #:sep ", "))]
        [(field ,c ,e0, e1)
         (if c
             (format "[~a] = ~a" (Expr e0) (Expr 1))
             (format "~a = ~a" (Expr e0) (Expr e1)))]
        [(index ,e0 ,e1)
         (format "~a[~a]" (Expr e0) (Expr e1))]
        [(access ,c ,e ,n)
         (format "~a~a~a" (Expr e) (if c ":" ".") n)]
        [(par ,e) ; parenthesized expr
         (format "(~a)" (Expr e))]
        [(,e* ... ,e)
         (format-list e e* #:sep ", ")])
  (Stmt : Stmt(ir) -> *()
        [(assign ,c (,e0* ... ,e0) (,e1* ...))
         (if (empty? e1*)
             (format "~a~a"
                     (if c "local " "")
                     (format-list e0 e0* #:sep ", "))
             (format "~a~a = ~a"
                     (if c "local " "")
                     (format-list e0 e0* #:sep ", ")
                     (format-list '() e1* #:sep ", ")))]
        [(if ,e ,s (,s* ...) ,s? ,c?)
         (begin
           (define els
             (match s?
               [#f ""]
               [es (format "~nelse ~n~a~n" (Stmt s?))]))
           (define eifs
             (string-join
              (for/list ([stmt s*])
                (format "~nelse~a" (Stmt stmt))) ""))
           (define is-end (if c? "end" ""))
           (format "if ~a then ~n ~a~a~a~a" (Expr e) (Stmt s) eifs els is-end))]
        [(while ,e ,s)
         (format "while ~a do ~n ~a ~nend" (Expr e) (Stmt s))]
        [(repeat ,s ,e)
         (format "repeat ~n ~a ~nuntil ~a" (Stmt s) (Expr e))]
        [(for ,n ,e0 ,e1 ,e2 ,s)
         (format "for ~a = ~a, ~a, ~a do ~n ~a ~nend" n (Expr e0) (Expr e1) (Expr e2) (Stmt s))]
        [(for (,n* ... ,n) ,e ,s)
         (format "for ~a in ~a do ~n ~a ~nend" (format-list n n* #:sep ", ") (Expr e) (Stmt s))]
        [(begin ,c (,s* ...))
         (begin
           (define stmts (format "~a~n" (format-list '() s* #:sep "\n")))
           (if c (format "do ~n ~a ~nend" stmts) stmts))]
        [(ret ,e* ...)
         (format "return ~a" (format-list '() e* #:sep ", "))]
        [(break)
         (format "break")]))

(provide parse-Lua parse-L1 new-cst->ast lower-op-assign generate-code parse)

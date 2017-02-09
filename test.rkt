#lang racket

(require "lexer.rkt")
(require "compiler.rkt")

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

;; MANUAL AST FOR TESTING OK FUC
(parse-L1 'x)
(parse-L1 '25)
(parse-L1 '(assign #t (x) (10)))
(parse-L1 '(assign #t (x y) (10 24)))
(parse-L1 '(op-assign #t ADD (x) (25)))
(lower-op-assign (parse-L1 '(op-assign #t ADD (x y z) (call print (25 32)))))
(parse-L1 '(op-assign #t ADD (x) (binop SUB 35 25)))
(lower-op-assign (parse-L1 '(op-assign #t ADD (x y) (24 (binop SUB 35 25)))))
(lower-op-assign (parse-L1 '(fn (x y z) (ret (32)))))
(displayln
 (generate-code (lower-op-assign (parse-L1 '(fn (a b c) (begin #f ((ret (32)))))))))

;; codegen testing
(displayln
 (generate-code
  (lower-op-assign
   (parse-L1 '(begin #t
                     ((assign #t (x) (0))
                      (while true (op-assign #f ADD (x) ((binop MUL 5 2))))))))))

(displayln
 (generate-code
  (lower-op-assign
   (parse-L1 '(begin #t
                     ((assign #t (x y) (0 0))
                      (while true (op-assign #f ADD (x y) ((binop MUL 32 16) 48)))
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
   (parse-L1 '(call (access #f (access #f love graphics) rectangle))))))

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

(define syntax-stuff
  "local x, y, z = 12, 24, 32
   local foo, bar = 10 + 24, 24 + 48
   local unary_foo, unary_bar = -foo, -bar
   local some_table = {12, 24, 48, {}, {25 / 32}}
   local this_thing_or_that_thing = (true and not 10) or 42
   some_string = \"hello, world!\"
   local what = false
   for k, v in pairs({1, 2, 3, 4}) do
     local wat = false
     print(k, v)
   end
   for i = 1, 10 do
     print \"yoink\"
     print {}
   end
   while what do
     local a, b, c = 1, 2, 3
   end
   if what then
     print \"yes what?\"
   elseif not what then
     print \"not what?\"
   elseif 25 then
   else
     print \"well then\"
   end
   if what then print \"nope\" end
   local explicit_func = function(but, why)
     local thing, other_thing = but, why
     shoot_things(\"hello, world!\", 12, 24, 32, {})
     bong.fucked(12, 24, 36)
   end
   function semicolon_things(a, b, c)
     local a = b; local b = c; local a = c;
     return a, b, c
   end
   local function argless_locality()
   end
   function less_variadic_things(...)
     local upvalue = false
     return function(...)
       return upvalue, ...
     end
   end
   hello.world = 25
   function something:shitty(a, b, c)
     return a, b, c
   end
   shooter:shoot_things(32):reduce(42)
   function more_variadic_things(f, g, h, ...)
     return 42
   end
   local function does_things(a, b, c)
     return 24
   end")

(define new-test-syntax
  (parse
   (tokenizer-thunk
    (open-input-string syntax-stuff))))

(define real-test-syntax
  (call-with-input-file "examples/timecop.lua"
    (lambda (ip)
      (parse
       (tokenizer-thunk ip)))))

(define (pretty-test name thunky)
  (displayln name)
  (pretty-print (thunky))
  (displayln ""))

(pretty-test "SYNTAX ->" (lambda () (syntax->datum real-test-syntax)))
(pretty-test "TEST 1 ->" (lambda () (parse-L1 '(assign #t (x y) (10 24)))))
(pretty-test "TEST 2 ->" (lambda () (new-cst->ast real-test-syntax)))
(pretty-test "TEST 3 ->" (lambda () (parse-L1 (new-cst->ast real-test-syntax))))
(pretty-test "TEST 4 ->" (lambda () (lower-op-assign (parse-L1 (new-cst->ast real-test-syntax)))))

(define example-code
 (generate-code
  (lower-op-assign
   (parse-L1 (new-cst->ast real-test-syntax)))))

(displayln example-code)

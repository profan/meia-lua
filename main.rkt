#lang racket

(require "lexer.rkt")
(require "compiler.rkt")

(define (time thunk)
  (define start (current-inexact-milliseconds))
  (define result (thunk))
  (values result (- (current-inexact-milliseconds) start)))

(define (pipeline ast)
  (lower-op-assign
   (parse-L1 ast)))

(define (compile fname)
  (define-values (cst cst-time)
    (call-with-input-file fname
      (lambda (ip)
        (define-values (tokens token-time) (time (lambda () (tokenizer-thunk ip))))
        (printf "tokenization time: ~a milliseconds ~n" token-time)
        (time (lambda () (parse tokens))))))
  (printf "cst parse time: ~a milliseconds ~n" cst-time)
  (define-values (ast ast-time) (time (lambda () (new-cst->ast cst))))
  (printf "cst -> ast took: ~a milliseconds ~n" ast-time)
  (define-values (lua-ast pipeline-time) (time (lambda () (pipeline ast))))
  (printf "hl-ast -> lua ast took: ~a milliseconds ~n" pipeline-time)
  (define-values (lua-code lua-time) (time (lambda () (generate-code lua-ast))))
  (printf "codegen took: ~a milliseconds ~n" lua-time)
  lua-code)

(define-values (example-code example-time) (time (lambda () (compile "examples/timecop.lua"))))
(printf "total time: ~a milliseconds to compile. ~n" example-time)

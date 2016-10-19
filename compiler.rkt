#lang nanopass

(require brag/support)
(require "grammar.rkt")

(define operators
  '(("+" 'ADD)
    ("-" 'SUB)
    ("*" 'MUL)
    ("/" 'DIV)
    ("^" 'POW)
    ("%" 'MOD)
    ("(" 'LPAREN)
    (")" 'RPAREN)
    ("{" 'LBRACKET)
    ("}" 'RBRACKET)
    ("[" 'LSQUARE)
    ("]" 'RSQUARE)
    ("<" 'LT)
    ("<=" 'LTEQ)
    (">" 'GT)
    (">=" 'GTEQ)
    ("==" 'EQEQ)
    ("~=" 'NEQ)
    ("..." 'VARIADIC)
    (".." 'CONCAT)
    (";" 'SEMICOLON)
    ("." 'PERIOD)
    ("," 'COMMA)
    ("#" 'LEN)
    ("=" 'EQ)))

(define keywords
  '(("if" 'IF)
    ("else" 'ELSE)
    ("elseif" 'ELSEIF)
    ("for" 'FOR)
    ("while" 'WHILE)
    ("repeat" 'REPEAT)
    ("until" 'UNTIL)
    ("in" 'IN)
    ("do" 'DO)
    ("then" 'THEN)
    ("end" 'END)
    ("break" 'BREAK)
    ("return" 'RETURN)
    ("function" 'FUNCTION)
    ("local" 'LOCAL)
    ("true" 'TRUE)
    ("false" 'FALSE)
    ("nil" 'NIL)
    ("and" 'AND)
    ("or" 'OR)
    ("not" 'NOT)))

(define (expand-operators ops)
  (for/fold ([str ""])
            ([op ops])
    (string-append str (format "\\~a" (car op)))))

(define (tokenize p)
  (define ex-ops (expand-operators operators))
  (for/list ([b-str (regexp-match* (pregexp (format "\"[^\"]+\"|(\\p{L}|\\_)+|\\p{N}+|[~a]" ex-ops)) p)])
    (define str (bytes->string/utf-8 b-str))
    (match str
      ["(" (token 'LPAREN str)]
      [")" (token 'RPAREN str)]
      ["[" (token 'LSQUARE str)]
      ["]" (token 'RSQUARE str)]
      ["{" (token 'LBRACKET str)]
      ["}" (token 'RBRACKET str)]
      ["if" (token 'IF str)]
      ["else" (token 'ELSE str)]
      ["elseif" (token 'ELSEIF str)]
      ["for" (token 'FOR str)]
      ["while" (token 'WHILE str)]
      ["repeat" (token 'REPEAT str)]
      ["until" (token 'UNTIL str)]
      ["in" (token 'IN str)]
      ["do" (token 'DO str)]
      ["then" (token 'THEN str)]
      ["end" (token 'END str)]
      ["break" (token 'BREAK str)]
      ["return" (token 'RETURN str)]
      ["function" (token 'FUNCTION str)]
      ["..." (token 'VARIADIC str)]
      ["local" (token 'LOCAL str)]
      ["true" (token 'TRUE str)]
      ["false" (token 'FALSE str)]
      ["nil" (token 'NIL str)]
      ["." (token 'PERIOD str)]
      ["," (token 'COMMA str)]
      ["+" (token 'ADD str)]
      ["-" (token 'SUB str)]
      ["*" (token 'MUL str)]
      ["/" (token 'DIV str)]
      ["^" (token 'POW str)]
      ["%" (token 'MOD str)]
      ["=" (token 'EQ str)]
      [".." (token 'CONCAT str)]
      ["<" (token 'LT str)]
      ["<=" (token 'LTEQ str)]
      [">" (token 'GT str)]
      [">=" (token 'GTEQ str)]
      ["==" (token 'EQEQ str)]
      ["~=" (token 'NEQ str)]
      ["and" (token 'AND str)]
      ["or" (token 'OR str)]
      ["not" (token 'NOT str)]
      ["#" (token 'LEN str)]
      [":" (token 'COLON str)]
      [";" (token 'SEMICOLON str)]
      [else
       (let* ([n (string->number str)])
         (match n
           [(? number?) (token 'NUM n)]
           [_
            (cond
              [(string-prefix? str "\"") (token 'STR (string-trim str "\""))]
              [else (token 'VAR str)])]))])))

(tokenize (open-input-string "for value, other_value in pairs({12, 24}) do 25 = 32 end"))
(tokenize (open-input-string "function test() return \"Hello, World!\" end"))
(tokenize (open-input-string "function test() local some_stuff = {1, 2, 3, 4}; return #some_stuff end"))
(tokenize (open-input-string "function hello_world() return false end"))
(tokenize (open-input-string "function hello_world() return not -25 end"))

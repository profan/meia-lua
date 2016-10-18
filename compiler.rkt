#lang nanopass

(require brag/support)
(require "grammar.rkt")

(define (tokenize p)
  (for/list ([str (regexp-match* #px"\"[^\"]+\"|(\\p{L}|\\_)+|\\p{N}+|[\\=\\,\\(\\)\\{\\}\\+\\-\\*\\/]" p)])
    (match str
      [#"(" (token 'LPAREN str)]
      [#")" (token 'RPAREN str)]
      [#"[" (token 'LSQUARE str)]
      [#"]" (token 'RSQUARE str)]
      [#"{" (token 'LBRACKET str)]
      [#"}" (token 'RBRACKET str)]
      [#"if" (token 'IF str)]
      [#"else" (token 'ELSE str)]
      [#"elseif" (token 'ELSEIF str)]
      [#"for" (token 'FOR str)]
      [#"while" (token 'WHILE str)]
      [#"repeat" (token 'REPEAT str)]
      [#"until" (token 'UNTIL str)]
      [#"in" (token 'IN str)]
      [#"do" (token 'DO str)]
      [#"then" (token 'THEN str)]
      [#"end" (token 'END str)]
      [#"break" (token 'BREAK str)]
      [#"return" (token 'RETURN str)]
      [#"function" (token 'FUNCTION str)]
      [#"..." (token 'VARIADIC str)]
      [#"local" (token 'LOCAL str)]
      [#"true" (token 'TRUE str)]
      [#"false" (token 'FALSE str)]
      [#"nil" (token 'NIL str)]
      [#"." (token 'PERIOD str)]
      [#"," (token 'COMMA str)]
      [#"+" (token 'ADD str)]
      [#"-" (token 'SUB str)]
      [#"*" (token 'MUL str)]
      [#"/" (token 'DIV str)]
      [#"^" (token 'POW str)]
      [#"%" (token 'MOD str)]
      [#"=" (token 'EQ str)]
      [#".." (token 'CONCAT str)]
      [#"<" (token 'LT str)]
      [#"<=" (token 'LTEQ str)]
      [#">" (token 'GT str)]
      [#">=" (token 'GTEQ str)]
      [#"==" (token 'EQEQ str)]
      [#"~=" (token 'NEQ str)]
      [#"and" (token 'AND str)]
      [#"or" (token 'OR str)]
      [#"#" (token 'LEN str)]
      [#":" (token 'COLON str)]
      [#";" (token 'SEMICOLON str)]
      [else
       (let* ([utf-str (bytes->string/utf-8 str)]
             [n (string->number utf-str)])
         (match n
           [(? number?) (token 'NUM n)]
           [_
            (cond
              [(string-prefix? utf-str "\"") (token 'STR (string-trim utf-str "\""))]
              [else (token 'VAR utf-str)])]))])))

(tokenize (open-input-string "for value, other_value in pairs({12, 24}) do 25 = 32 end"))
(tokenize (open-input-string "function test() return \"Hello, World!\" end"))
(tokenize (open-input-string "function test() some_stuff = {1, 2, 3, 4}; return #some_stuff end"))
(tokenize (open-input-string "function hello_world() return false end"))

#lang brag

;; this file defines the Lua grammar as a brag grammar, for use as reference once it can parse all of Lua, which we will then build on to add our own desired additions to the language!

;; lua grammar

chunk: (stat [SEMICOLON])* [laststat [SEMICOLON]]

block: chunk

stat: varlist EQ explist
    | functioncall
    | DO block END
    | WHILE exp DO block END
    | REPEAT block UNTIL exp
    | IF exp THEN block (ELSEIF exp THEN block)* [ELSE block] END
    | FOR VAR EQ exp COMMA exp [COMMA exp] DO block END
    | FOR namelist IN explist DO block END
    | FUNCTION funcname funcbody
    | LOCAL FUNCTION VAR funcbody
    | LOCAL namelist [EQ explist]

laststat: RETURN [explist] | BREAK

funcname: VAR (DOT VAR)* [COLON VAR]

varlist: var (COMMA var)*

var: VAR | prefixexp LBRACKET exp RBRACKET | prefixexp PERIOD VAR

namelist: VAR (COMMA VAR)*

explist: (exp COMMA)* exp

exp: NIL | FALSE | TRUE | NUM | STR | VARIADIC | function | prefixexp | tableconstructor | exp binop exp | unop exp

prefixexp: var | functioncall | LPAREN exp RPAREN

functioncall: prefixexp args | prefixexp COLON VAR args

args: LPAREN [explist] RPAREN | tableconstructor | STR

function: FUNCTION funcbody

funcbody: LPAREN [parlist] RPAREN block END

parlist: namelist [COMMA VARIADIC] | VARIADIC

tableconstructor: LBRACKET [fieldlist] RBRACKET

fieldlist: field (fieldsep field)* [fieldsep]

field: LBRACKET exp RBRACKET EQ exp | VAR EQ exp | exp

fieldsep: COMMA | SEMICOLON

binop: ADD | SUB | MUL | DIV | POW | MOD | CONCAT | LT | LTEQ | GT | GTEQ | EQEQ | NEQ | AND | OR

unop: SUB | NOT | LEN

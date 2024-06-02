
(* The type of tokens. *)

type token = 
  | TRUE
  | TIMES
  | THEN
  | SEMI
  | RPAREN
  | REF
  | PLUS
  | OR
  | NEQ
  | MINUS
  | LT
  | LPAREN
  | LET
  | LEQ
  | INT of (int)
  | IN
  | IF
  | IDENT of (string)
  | GT
  | GEQ
  | FUN
  | FALSE
  | EQ
  | EOF
  | ELSE
  | DIV
  | BANG
  | ASSIGN
  | ARR
  | AND

(* This exception is raised by the monolithic API functions. *)

exception Error

(* The monolithic API. *)

val prog: (Lexing.lexbuf -> token) -> Lexing.lexbuf -> (Ast.expr)

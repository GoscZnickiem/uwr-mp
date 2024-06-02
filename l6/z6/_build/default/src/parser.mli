
(* The type of tokens. *)

type token = 
  | TRUE
  | TIMES
  | THEN
  | RPAREN
  | PLUS
  | MINUS
  | LPAREN
  | LEQ
  | LE
  | INT of (int)
  | IF
  | GRQ
  | GR
  | FALSE
  | EQ
  | EOF
  | ELSE
  | DIV
  | DIF

(* This exception is raised by the monolithic API functions. *)

exception Error

(* The monolithic API. *)

val prog: (Lexing.lexbuf -> token) -> Lexing.lexbuf -> (Ast.expr)
